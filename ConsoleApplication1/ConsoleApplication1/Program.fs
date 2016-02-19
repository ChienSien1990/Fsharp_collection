#light

// this is the max function
let rec max l =                 
    match l with
    | [] -> 0
    | x::xs when x>max xs ->x   
    | x::xs -> max xs

// this is the selected index function
let rec nth e l =
    match l with
    | x::xs when e=0->x
    | x::xs -> nth (e-1) xs

// this is the zip function
let rec zip (x,y)  = 
   match x,y with
    | (_,[])->[]
    | ([],_)->[]
    | (xhd::xtl, yhd::ytl)->  (xhd,yhd)::zip(xtl,ytl)


// this is the search function
let rec search f e = 
    match e with
    | [] -> -1
    | x::xs ->  if f x = true then 0                    // if the head is true then break and equals to index
                else  if search f xs= -1 then -1        // if the search is false then it always be -1
                else search f xs + 1 
 

