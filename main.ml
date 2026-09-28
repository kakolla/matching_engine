
type order_type = Market | Limit of int ;;

type position = Buy | Sell;;


(* last integer is 0 if it is a market order, or the actual limiting price if it is a limit order*)
type order = Order of string * order_type * position;;    (* Replace unit with your solution. *)


let market_buy_order (customer : string) : order =
        Order(customer, Market, Buy);;

let limit_buy_order (customer : string) (price : int) : order =
        Order(customer, Limit price , Buy);;

let market_sell_order (customer : string) : order =
        Order(customer, Market, Sell);;

let limit_sell_order (customer : string) (price : int) : order =
        Order(customer, Limit price, Sell);;


(* list of orders, current price of the stock *)
(* buys and sells *)

type order_book = {
        buys : order list;
        sells: order list;
        curr_price: int option;
        time: int; (* for the time - FIFO*)

}

let empty_book : order_book = {
        buys = [];
        sells = [];
        curr_price = None;
        time = 0;
}


(* return the updated order book as well as the orders that occur*)
let accept_order (o: order) (b : order_book) : (order * order) option * order_book =
        match o with 
        | Order(_, Market, Buy) -> (match b.sells with 
        (* we have to buy, so find the lowest seller*)
                        |  [] ->  (None, b)
                        | (Order (_, Limit p, _) as lowest) :: rest -> Some((o, lowest)), {b with sells=rest; curr_price= Some p}
                        | _ :: _ -> (None, b)
        )
        (* we have to sell , so find the highest buyer *)
                        | Order(_, Market, Sell) -> ( match b.buys with
                        | [] -> (None, b)
                        | (Order (_, Limit p, _) as highest) :: rest -> Some((highest, o)), {b with buys=rest; curr_price = Some p}
                        | _ :: _ -> (None, b)

                        )
;;
  

