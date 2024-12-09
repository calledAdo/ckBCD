import Nat "mo:base/Nat";
import ExperimentalCycles "mo:base/ExperimentalCycles";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Result "mo:base/Result";
import Nat64 "mo:base/Nat64";
import Nat32 "mo:base/Nat32";
import Types "Types";

shared ({ caller }) actor class QuoteMinter(_priceFeedID : Text, solvencyFacor : Nat, _ckBTCPrincipal : Text, _quoteID : Text, _stathID : Text) = this {
  type Aggregator = Types.Aggregator;
  type PriceFeed = Types.XRC;
  type Subaccount = Types.Subaccount;
  type Result<Err, Ok> = Result.Result<Text, Text>;
  type Price = {
    price : Nat;
    price_decimal : Nat;
  };
  type PriceResult = {
    #Ok : Price;
    #Err : Text;
  };
  type Details = {
    btcPrice : Nat;
    priceDecimal : Nat;
    poolBalance : Nat;
    poolValuation : Nat;
    quoteTotalSupply : Nat;
    stathTotalSupply : Nat;
  };
  type Essentials = {
    #Ok : Details;
    #Err : Text;
  };

  stable var incentiveFactor = 0;
  stable var solvencyFactor = 0;
  stable var deployed = false;
  stable var bootStrapPhase = true;
  stable var admin = caller;
  stable let priceFeedID = _priceFeedID;
  stable let ckBTCPrincipal = _ckBTCPrincipal;
  stable let quotePrincipal = _quoteID;
  stable let stathPrincipal = _stathID;
  stable let quote : Types.ICRC = actor (quotePrincipal);
  stable let stath : Types.ICRC = actor (stathPrincipal);
  stable let ckBTC : Types.ICRC = actor (ckBTCPrincipal);
  stable let btcPriceAggregator : Aggregator = actor ("aaaaa-aa");
  stable let oracle : PriceFeed = actor (priceFeedID);

  private func _incentiveValue(liquidityDifference : Nat, amountIn : Nat) : Nat {
    let ivalue = (incentiveFactor * liquidityDifference * amountIn) / 10 ** 18;
    return ivalue;
  };

  private func _sendIn(tokenID : Text, isTransfer : Bool, from : Principal, _subaccount : ?Subaccount, amount : Nat) : async () {
    let token : Types.ICRC = actor (tokenID);
    let actorAddress = Principal.fromText("");
    var fee = 0;
    if (isTransfer) {
      fee := await token.icrc1_fee();
    };
    let tx = await ckBTC.icrc2_transfer_from({
      spender_subaccount = null;
      from = {
        owner = from;
        subaccount = _subaccount;
      };
      to = {
        owner = actorAddress;
        subaccount = null;
      };
      amount = amount;
      fee = ?fee;
      memo = null;
      created_at_time = null;

    });
    let result = switch (tx) {
      case (#Ok(result)) { true };
      case (#Err(err)) { false };
    };
    assert (result);
  };

  private func _sendOut(tokenID : Text, isTransfer : Bool, to : Principal, _subaccount : ?Subaccount, amount : Nat) : async () {
    let token : Types.ICRC = actor (tokenID);
    var fee = 0;
    if (isTransfer) {
      fee := await token.icrc1_fee();
    };
    let tx = await token.icrc1_transfer({
      from_subaccount = null;
      to = { owner = to; subaccount = _subaccount };
      amount = amount;
      fee = ?fee;
      memo = null;
      created_at_time = null;
    });
    let result = switch (tx) {
      case (#Ok(result)) { true };
      case (#Err(err)) { false };
    };
    assert (result);
  };

  private func _takeFee(amount : Nat, fee : Nat) : Nat {
    let diff = amount * fee / 100000;
    return amount - diff;
  };
  private func _percent(amount : Nat, percent : Nat) : Nat {
    return (amount * percent) / 100000;
  };

  private func _getExchangeRate() : async PriceResult {
    ExperimentalCycles.add(1_000_000_000);
    let rateResult = await oracle.get_exchange_rate({
      base_asset = {
        symbol = "BTC";
        class_ = #Cryptocurrency;
      };
      quote_asset = {
        symbol = "USD";
        class_ = #FiatCurrency;
      };
      timestamp = null;
    });

    let result = switch (rateResult) {
      case (#Ok(exchangeRate)) {
        exchangeRate;
      };
      case (#Err(err)) {
        return #Err("could");
      };
    };

    #Ok({
      price = Nat64.toNat(result.rate);
      price_decimal = Nat32.toNat(result.metadata.decimals);
    });
  };

  private func _getEssentials() : async Essentials {
    let rateResult = await _getExchangeRate();

    let result : Price = switch (rateResult) {
      case (#Ok(price)) { price };
      case (#Err(err)) { return #Err(err) };
    };
    let price = result.price;
    let priceDecimal = result.price_decimal;
    let poolBalance = await ckBTC.icrc1_balance_of({
      owner = Principal.fromText("aaaaa-aa");
      subaccount = null;
    });

    return #Ok({
      btcPrice = result.price;
      priceDecimal = result.price_decimal;
      poolBalance = poolBalance;
      poolValuation = (poolBalance * price) / priceDecimal;
      quoteTotalSupply = await quote.icrc1_total_supply();
      stathTotalSupply = await stath.icrc1_total_supply();
    });
  };

  public func init() : async () {
    assert (deployed == false);
    deployed := true;
  };

  private func _mintSTATH(caller : Principal, _subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    let details : Details = switch (await _getEssentials()) {
      case (#Ok(essentials)) { essentials };
      case (#Err(err)) { return #err(err) };
    };
    let quoteValuation = (details.quoteTotalSupply * 10 ** details.priceDecimal) / details.btcPrice;
    if (details.poolBalance < quoteValuation) {
      return await _bootStrapMint(caller, _subaccount, amount);
    };
    let amountToMint : Nat = amount * details.stathTotalSupply / (details.poolBalance - quoteValuation);
    let amountValue = amountToMint * details.btcPrice / 10 ** details.priceDecimal;
    if ((details.quoteTotalSupply * 8) < (details.poolValuation + amountValue)) {
      return #err("Exceeded Collateral Maximum");
    };
    let sendInTx = await _sendIn(ckBTCPrincipal, true, caller, _subaccount, amount);
    let mintTx = await _sendOut(stathPrincipal, false, caller, _subaccount, amountValue);
    return #ok("Successful");
  };

  private func _bootStrapMint(caller : Principal, _subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    let actorAddress = Principal.fromText("");
    let fee = await ckBTC.icrc1_fee();
    let sendInTX = await _sendIn(ckBTCPrincipal, true, caller, _subaccount, amount);
    let mintTx = await _sendOut(stathPrincipal, false, caller, _subaccount, amount);
    return #ok("Successful");

  };

  private func _mintQUOTE(caller : Principal, _subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    let details : Details = switch (await _getEssentials()) {
      case (#Ok(essentials)) { essentials };
      case (#Err(err)) { return #err(err) };
    };

    let amountEquivalent = amount * details.btcPrice / 10 ** details.priceDecimal;
    if ((details.poolValuation / 4) < (details.quoteTotalSupply + amountEquivalent)) {
      return #err("Insufficient Collateral in Pool");
    };
    let amountAfterFees = _takeFee(amountEquivalent, 500);
    let sendInTx = await _sendIn(ckBTCPrincipal, true, caller, _subaccount, amount);
    let mintTx = await _sendOut(quotePrincipal, false, caller, _subaccount, amountAfterFees);
    return #ok("Succesful");
  };

  private func _burnSTATH(caller : Principal, _subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    let details : Details = switch (await _getEssentials()) {
      case (#Ok(essentials)) { essentials };
      case (#Err(err)) { return #err(err) };
    };
    let quoteValuation = (details.quoteTotalSupply * (10 ** details.priceDecimal)) / details.btcPrice;
    let amountEquivalent : Nat = amount * (details.poolBalance - quoteValuation) / details.stathTotalSupply;
    let poolValuationAfter : Nat = ((details.poolBalance - amountEquivalent) * details.btcPrice) / 10 ** details.priceDecimal;
    if ((details.quoteTotalSupply * 4) < poolValuationAfter) {
      return #err("Minimum Collateral Subceeded");
    };
    let amountAfterFees = _takeFee(amountEquivalent, 10 ** 2);
    let burnTx = await _sendIn(stathPrincipal, false, caller, _subaccount, amount);
    let sendOutTx = await _sendOut(ckBTCPrincipal, false, caller, _subaccount, amountAfterFees);
    return #ok("Succesful");
  };

  private func _burnQUOTE(caller : Principal, _subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    let details : Details = switch (await _getEssentials()) {
      case (#Ok(essentials)) { essentials };
      case (#Err(err)) { return #err(err) };
    };
    let amountEquivalent = (amount * (10 ** details.priceDecimal)) / details.btcPrice;
    let amountAfterFees = _takeFee(amountEquivalent, 500);
    let burnTx = await _sendIn(quotePrincipal, false, caller, _subaccount, amount);
    let sendOutTx = await _sendOut(ckBTCPrincipal, false, caller, _subaccount, amountAfterFees);
    return #ok("Successful");
  };

  private func _iBurnQUOTE(caller : Principal, _subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    let details : Details = switch (await _getEssentials()) {
      case (#Ok(essentials)) { essentials };
      case (#Err(err)) { return #err(err) };
    };
    let threshhold = _percent(details.quoteTotalSupply, solvencyFactor);
    if (threshhold < details.poolValuation) {
      return #err("Solvency Threshhold not Subceeded");
    };
    let quoteValuation = (details.quoteTotalSupply * 10 ** details.priceDecimal) / details.btcPrice;
    let amountEquivalent : Nat = amount * (details.poolBalance - quoteValuation) / details.quoteTotalSupply;
    let amountAfterFees = _takeFee(amountEquivalent, 500);
    let burnTX = await _sendIn(quotePrincipal, false, caller, _subaccount, amount);
    let sendOutTx = await _sendOut(ckBTCPrincipal, false, caller, _subaccount, amountAfterFees);
    let ivalue = _incentiveValue(threshhold - details.poolValuation, amount);
    let mintTx = await _sendOut(stathPrincipal, false, caller, _subaccount, ivalue);
    return #ok("Succesfull");
  };

  public shared ({ caller }) func mintSTATH(_subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    assert (bootStrapPhase == false);
    let Tx = await _mintSTATH(caller, _subaccount, amount);
  };

  public shared ({ caller }) func bootStrapMint(_subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    assert (bootStrapPhase == true);
    let Tx = await _bootStrapMint(caller, _subaccount, amount);
  };

  public shared ({ caller }) func mintQUOTE(_subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    assert (bootStrapPhase == false);
    let Tx = await _mintQUOTE(caller, _subaccount, amount);
  };

  public shared ({ caller }) func burnSTATH(_subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    assert (bootStrapPhase == false);
    let Tx = await _burnSTATH(caller, _subaccount, amount);
  };

  public shared ({ caller }) func burnQUOTE(_subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    assert (bootStrapPhase == false);
    let Tx = await _burnQUOTE(caller, _subaccount, amount);

  };

  public shared ({ caller }) func iBurnQUOTE(_subaccount : ?Subaccount, amount : Nat) : async Result<Text, Text> {
    assert (bootStrapPhase == false);
    let Tx = await _iBurnQUOTE(caller, _subaccount, amount);
  };

  public shared ({ caller }) func setPhase(phase : Bool) : async () {
    assert (caller == admin);
    bootStrapPhase := phase;
  };
  public shared ({ caller }) func setIncentiveFactor(factor : Nat) : async () {
    assert (caller == admin);
    incentiveFactor := factor;
  };

  public shared ({ caller }) func setSolvencyFactor(factor : Nat) : async () {
    assert (caller == admin);
    solvencyFactor := factor;
  };

  public shared ({ caller }) func setNewAdmin(newAdmin : Principal) : async () {
    assert (caller == admin);
    admin := newAdmin;
  };

  public query func getIncentiveFactor() : async Nat {
    return incentiveFactor;
  };

  public query func getSolvencyFactor() : async Nat {
    return solvencyFactor;
  };
};
