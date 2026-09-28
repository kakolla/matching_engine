



type order_type = Market | Limit ;;

type position = Buy | Sell;;


(* last integer is 0 if it is a market order, or the actual limiting price if it is a limit order*)
type order = Order of string * order_type * position *  int;;    


let market_buy_order (customer : string) : order =
        Order(customer, Market, Buy, 0);;

let limit_buy_order (customer : string) (price : int) : order =
        Order(customer, Limit, Buy, price);;

let market_sell_order (customer : string) : order =
        Order(customer, Market, Sell, 0);;

let limit_sell_order (customer : string) (price : int) : order =
        Order(customer, Limit, Sell, price);;


(* type order_book =  *)
