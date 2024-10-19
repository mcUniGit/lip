let rec lang1 = function  
 |['0']-> true
 |['1'] -> true
 |'0'::l -> lang1 l 
 |'1'::l -> lang1 l 
 |_ -> false;;   

let rec lang2 = function
  [] |['0'] |['1'] -> true
 |'0'::l -> lang2 l 
 |'1'::l -> lang2 l 
 |_ -> false

let lang3 l = match l with
    [] -> false
    |'0'::_ ->
            let rec check = function
                '0'::['0'] |'1'::['0'] -> true 
                |'0'::l |'1'::l -> check l
                |_ -> false in check l
    |_ -> false

let lang4 l =  
    let rec check l o = match l with
        [] -> o > 1
        |'0'::l -> check l o
        |'1'::l -> check l (o+1) 
        |_ -> false in check l 0 

let rec lang5 = function 
    ['0';'0'] |['1';'1'] -> true
    |'0'::'0'::l | '1'::'1'::l -> lang5 l 
    |_ -> false

let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers;;

belongsTo ['1';'0';'0';'1'];; 
  
