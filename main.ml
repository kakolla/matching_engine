
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

(* add the buy to sorted position of the list of buys*)


(* hlper func - return price of order *)
let getprice (Order(_, ordertype, _)) = match ordertype with
| Market -> 0
| Limit p -> p;;
        

let rec add_sorted_buy (o: order) (buys: order list) = 
        match buys with
        | [] -> [o]
        | hd :: tl -> if getprice o > getprice hd then o :: buys 
        else hd :: add_sorted_buy o tl ;;

let rec add_sorted_sell (o: order) (sells: order list) = 
        match sells with
        | [] -> [o]
        | hd :: tl -> if getprice o < getprice hd then o :: sells 
        else hd :: add_sorted_sell o tl ;;




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

        | Order(_, Limit p, Buy) -> (match b.sells with 
                        | [] -> (None, {b with buys= add_sorted_buy o b.buys})
                        | lowest :: rest -> if getprice lowest <= p then (Some((o, lowest)), {b with sells=rest; curr_price = Some(getprice lowest)}) else 
                                (None, {b with buys = add_sorted_buy o b.buys}))

        (* this is a limit sell, try to find the highest buyer over our min limit *)
        | Order(_, Limit p, Sell) -> (match b.buys with 
                        | [] -> (None, {b with sells=add_sorted_sell o b.sells})
                        | highest :: rest -> if getprice highest >= p then (Some((highest, o)), {b with buys=rest; curr_price = Some(getprice highest)}) else
                                (None, {b with sells = add_sorted_sell o b.sells})

        )

;;
