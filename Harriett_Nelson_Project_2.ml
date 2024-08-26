(* function to read the fives.txt file supplied in class*) 
let read_lines name : string list =  
  let file = open_in name in
  let try_read () = 
    try Some (input_line file) with End_of_file -> None in
  let rec read_loop file_list = match try_read () with
    | Some s -> read_loop (s::file_list)  (* could have done: file_list@[s]*)
    | None -> close_in file ; file_list in 
  read_loop [] 

(*Wordle Project *) 
let wordleList = ["apple"; "apron"; "learn"; "paper"; "peppy"; "pepsi"];; (*this is the test of wordlelist for the online complier*) 

let explode s = s |> String.to_seq |> List.of_seq;; 

(* This will go through the list and get ride of any blank string spaces, for example: "" ,will be removed*)
let emptyCheck input = 
  let input = input in
  List.fold_right(fun elt accum -> if (elt <> "") then elt::accum else accum) input [];;

(*This checks to see if the list has the item*)
let rec contains ls bool char = 
  match ls with
  |[] -> bool
  |(head::rest) -> if(head == char) then
        contains ([]) (true) (char)
      else
        contains (rest) (false) (char)
          
(*This turns the characters to integers. This allows for us to use them as indexes*)     
let charToInt char= 
  if char == '0' 
  then 0
  else if char == '1'
  then 1 
  else if char == '2'
  then 2 
  else if char == '3'
  then 3
  else 4


(*Grabs the letters from the user input, and separates the letters and numbers into the strings*)
let splitWord input = 
  let wordlelist = explode input in
  List.fold_right(fun elt accum -> if (elt != '0') && (elt != '1') && (elt != '2') then elt::accum else accum) wordlelist [];;

(*Same thing as above but with the numbers*)
let splitNum input =
  let numList = explode input in
  List.fold_right(fun elt accum -> if (elt == '0') || (elt == '1') || (elt == '2') then elt ::accum else accum) numList [];; 


(*Put every input in wordleList into usable format, and calls the above splitWord*)
let rec splitWordInput input ls =
  match input with
  | [] -> ls
  | (head::rest) -> let word = splitWord head in
      let newWordList = (word :: ls) in
      splitWordInput(rest) (newWordList);; 

(*Put every input in wordleList into usable format, and calls the above splitNum*)
let rec splitNumInput input ls =
  match input with
  | [] -> ls
  | (head::rest) -> let num = splitNum head in
      let newNumList = (num :: ls) in
      splitNumInput(rest) (newNumList);;

(*The function makeZeroList2 goes through the certain index of the indexlist and putting anything with a zero index into the zeroList*)
let rec makeZeroList2 indexList numList ls count = 
  match indexList with
  | []-> ls
  | (head::rest) -> if ((List.nth numList count)=='0') 
      then makeZeroList2(rest) (numList) (String.make 1 head^ls) (count+1) 
      else makeZeroList2(rest) (numList) (ls) (count+1);;


(*Sort word chars into lists based on correctness and index*)
let rec makeZeroList wordleList numList ls count =
  match wordleList with
  | [] -> explode ls
  |(head::rest) ->let intListTwo = List.nth numList count in
      let temp =makeZeroList2(head) (intListTwo) ("") (0) in
      makeZeroList(rest) (numList) (temp^ls) (count+1);;


(*The function makeOneList2 goes through the certain index of the indexlist and putting anything with a one index into the oneList*)
let rec makeOneList2 indexList numList ls count =
  match indexList with
  | []-> ls
  | (head::rest) -> if ((List.nth numList count)=='1') 
      then makeOneList2(rest) (numList) (String.make 1 head^string_of_int (count)^ls) (count+1) 
      else makeOneList2(rest) (numList) (ls) (count+1);;

(*Sort word chars into lists based on correctness and index*)
let rec makeOneList wordleList numList ls count =
  match wordleList with
  | [] -> explode ls
  |(head::rest) ->let intListTwo = List.nth numList count in
      let temp =makeOneList2(head) (intListTwo) ("") (0) in
      makeOneList(rest) (numList) (temp^ls) (count+1);;


(*The function makeTwoList2 goes through the certain index of the indexlist and putting anything with a two index into the twoList*)
let rec makeTwoList2 indexList numList ls count =
  match indexList with
  | []-> ls
  | (head::rest) -> if ((List.nth numList count)=='2') 
      then makeTwoList2(rest) (numList) (String.make 1 head^string_of_int (count)^ls) (count+1) 
      else makeTwoList2(rest) (numList) (ls) (count+1);;

(*Sort word chars into lists based on correctness and index*)
let rec makeTwoList wordleList numList ls count =
  match wordleList with
  | [] -> explode ls
  |(head::rest) ->let intListTwo = List.nth numList count in
      let temp =makeTwoList2(head) (intListTwo) ("") (0) in
      makeTwoList(rest) (numList) (temp^ls) (count+1);;

(*Check the qualifications of the two List*)
let rec checkTwoList2 wordleList twoList count = 
  if count >= (List.length(twoList)-1)
  then wordleList
  else
    let letter = List.nth twoList count in  
    let number = List.nth twoList (count+1) in 
    let location = charToInt(number) in 
    let wordleWord = explode wordleList in
    if (List.nth wordleWord location)==(letter)
    then checkTwoList2(wordleList)(twoList)(count+2) 
    else checkTwoList2("")(twoList)(List.length(twoList)-1) 
        
        
(*Check the qualifications of the one List*)
let rec checkOneList2 wordleList oneList count =
  if count >= (List.length(oneList)-1)
  then wordleList
  else
    let letter = List.nth oneList count in  
    let number = List.nth oneList (count+1) in 
    let location = charToInt(number) in 
    let wordleWord = explode wordleList in
    if (List.nth wordleWord location)!=(letter) && contains(wordleWord) (false) (letter)
    then checkOneList2(wordleList)(oneList)(count+2) 
    else checkOneList2("")(oneList)(List.length(oneList)-1)
        
(*Function called to eventually look at checkOneList2 but goes through the list of the exploded word*)        
let rec checkOneList wordleList oneList ls count =
  match wordleList with
  | [] -> ls
  | (head::rest) -> 
      let checked = checkOneList2(head) oneList 0 in 
      checkOneList (rest) (oneList)(checked::ls) (0);;
        
(*Check the qualifications of the Zero List*)
let rec checkZeroList2 wordleList zeroList count =
  if count > (List.length(zeroList)-1)
  then wordleList
  else
    let letter = List.nth zeroList count in
    let wordleWord = explode wordleList in
    if (contains(wordleWord) (false) (letter) == true)
    then checkZeroList2("") (zeroList) (List.length(zeroList)-1)
    else checkZeroList2 (wordleList) (zeroList) (count+1) 
        
(*Function called to eventually look at checkZeroList2 but goes through the list of the exploded word, once done it will go to checkOneList*)          
let rec checkZeroList wordleList zeroList oneList ls count =
  match wordleList with
  | [] -> checkOneList (emptyCheck(ls)) (oneList) ([]) 0
  | (head:: rest) ->
      let checked = checkZeroList2 (head) zeroList 0 in
      checkZeroList (rest) (zeroList) (oneList) (checked::ls) (0);;

(*Function called to eventually look at checkTwoList2 but goes through the list of the exploded word, once done it will go to checkZeroList*)  
let rec checkTwoList wordleList twoList zeroList oneList ls count =
  match wordleList with
  | [] -> checkZeroList (emptyCheck (ls)) (zeroList) (oneList)([]) 0 
  | (head::rest) -> 
      let checked = checkTwoList2(head) twoList 0 in 
      checkTwoList(rest) (twoList) (zeroList) (oneList) (checked::ls) (0);;

(*This is where the project will actually do its job, with all the methods we have created, we only need to call checkTwoList at the end because 
  within that method it will go through the checkZeroList and then the checkOneList*)
let wordleSolver input = 
  let wordleList = read_lines "fives.txt" in
  let wordList = splitWordInput (input) ([]) in
  let numList = splitNumInput (input) ([]) in
  let twoList = makeTwoList (wordList) (numList) ("") (0) in
  let zeroList = makeZeroList (wordList) (numList) ("") (0) in
  let oneList = makeOneList (wordList) (numList) ("") (0) in
  checkTwoList (wordleList) (twoList) (zeroList) (oneList) ([]) 0
    



    
  
        















