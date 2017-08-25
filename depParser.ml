(*
  Implementation of Nivre's parsing algorithm, for Spanish, English, and Portuguese.
  All languages use the same set of dependency rules.
*)


(*Parts of Speech*)
type pos = Adv | Adj | N | Aux | V | P | Det | Num | Root
(*Rules are listed with their head first*)
(*A dependency rule can be a left rule (allowing a left arc) or a right rule (allowing a right arc)*)
type dRule = LeftRule of pos * pos * string | RightRule of pos * pos * string
(*Words are annotated with their parts of speech*)
type word = Word of string * pos
(*First word is head, second is dependent*)
type arc = LeftArc of word * word | RightArc of word * word

(*The dependency rules shared across all languages used*)
let languageRuleList = [
  LeftRule (Adj,Adv,""); (*avmod*)
  LeftRule (N,Adj,""); (*amod*)
  RightRule (N,Adj,""); (*amod-spa, por*)
  LeftRule (V,Aux,""); (*aux*)
  LeftRule (N,P,""); (*case*)
  LeftRule (N,Det,""); (*det*)
  RightRule (V,N,""); (*dobj*)
  RightRule (V,N,""); (*iobj*)
  LeftRule (V,N,""); (*nsubj*)
  RightRule (V,N,""); (*nsubj*)
  LeftRule (N,Num,""); (*nummod*)
  RightRule (Root,V,""); (*root*)
]

(*helper function to return first word in a list*)
let lsthd lst =
  match lst with
  | [] -> Word ("",N)
  | hd :: tl -> hd

(*helper function to determine if a word is the "empty word" *)
let empty w =
  if (w = Word ("",N)) then true else false

(*helper function to return the tail of a list*)
let lsttl lst =
  match lst with
  | [] -> []
  | hd :: tl -> tl

(*helper function to return the list length*)
let length lst =
  let rec helper lst acc =
    match lst with
    | [] -> acc
    | hd :: tl -> helper tl (acc+1)
  in
  helper lst 0

(*helper function to extract the part of speech of a word*)
let getPos word =
  match word with
  | Word (_,pos) -> pos
  | _ -> failwith "this is not a word"

(*pops the top of the stack*)
let reduce lst stack graph =
  (lst,(lsttl stack),graph)

(*adds left arc from input word to top of stack, reduces stack, leaves word on the list*)
let leftArc lst stack graph =
  let w = lsthd lst in
  let s = lsthd stack in
  if not (empty w && (empty s)) then
    let a = LeftArc (w,s) in
    (lst, (lsttl stack), (a :: graph))
  else
    failwith "leftArc problem"


(*adds right arc from stack to input word, pushes word onto stack*)
(*removes word from list*)
let rightArc lst stack graph =
  let w = lsthd lst in
  let s = lsthd stack in
  if not (empty w && (empty s)) then
    let a = RightArc (s,w) in
    let newS = w :: stack in
    ((lsttl lst), newS, (a :: graph))
  else
    failwith "rightArc problem"


(*adds first word onto stack*)
let shift lst stack graph =
  let w = lsthd lst in
  if not (empty w) then
    let newLst = lsttl lst in
    let newStack = w :: stack in
    (newLst,newStack,graph)
  else
    failwith "shift problem"

(*references to keep track of efficiency of the parse*)
 let count = ref 0
 let reset = ref 0

(*Returns true if the dependency is in the set, false otherwise*)
let rec findDependency dependency set =
  match set with
  |[] -> false (*there is no dependency with that head and dependent*)
  | hd :: tl when hd = dependency -> true
  | hd :: tl -> findDependency dependency tl

(*determines if conditions are met for arcs, etc, then tries them in
  left arc -> right arc -> reduce -> shift
  order*)
(*Recursive function that calls itself until the final state is achieved, or
  the parse is deemed impossible*)
(*Keeps track of state at each step to be able to backtrack and try the next
  level of function on a failed parse attempt*)
let rec oracle lst stack graph depSet state level =
  (*returns true if w has no head in current graph, false otherwise*)
  let rec headHelper w graph =
    match graph with
    |[] -> true
    | LeftArc (word, wp) :: tl when wp = w -> false
    | RightArc (word, wp) :: tl when wp = w -> false
    | hd::tl -> headHelper w tl
  in
  let w = lsthd lst in
  let s = lsthd stack in
  (*check conditions a left arc*)
  (*Condition: First word in stack has no head in current graph
               stack and word list are not empty
               "level" = 0; left arc has not been tried before
               required dependency exists*)
  let left = LeftRule ((getPos w),(getPos s),"") in
  if (level = 0 && not (empty w) && not (empty s) && (headHelper s graph) && findDependency left depSet) then
    let (l,s,g) = leftArc lst stack graph in
    let state = (lst,stack,graph,1)::state in
    (count := !count + 1);
    oracle l s g depSet state 0
  else
    (*check conditions for a right arc*)
    (*Condition: first word in sentence has no head in current graph
                 stack and word list are not empty
                 right arc has not been tried before
                 required dependency exists*)
    let right = RightRule ((getPos s),(getPos w),"") in
    if (level <= 1 && not (empty w) && not (empty s) && (headHelper w graph) && findDependency right depSet) then
      let (l,s,g) = rightArc lst stack graph in
      let state = (lst,stack,graph,2)::state in
      (count := !count + 1);
      oracle l s g depSet state 0
    else
      (*check conditions for a reduce*)
      (*Condition: top of stack has head in graph
                   stack is not empty
                   reduce has not been tried before*)
      if (level <= 2 && not (empty s) && (not (headHelper s graph))) then
        let (l,s,g) = reduce lst stack graph in
        let state = (lst,stack,graph,3)::state in
        (count := !count + 1);
        oracle l s g depSet state 0
      (*execute a shift operation, if possible. Requires word list to be nonempty*)
      else if not (empty w) then
        let (l,s,g) = shift lst stack graph in
        (count := !count + 1);
        oracle l s g depSet state 0
      else (*no more words, but no conditions fit.*)
      (*if Root is the only thing on the stack, done! output arc set*)
        if (s = Word("Root",Root)) then
          (!count,!reset,graph) (*Returns #steps, #backtracks, arc set*)
        else (*If there is a previous state to return to, try parsing from there*)
          match state with
          |[] -> failwith "no valid parse"
          |(l,s,g,x)::tl -> (reset := !reset +1); oracle l s g depSet tl x

(*A wrapper to begin the dependency parse. lst is a list of Words, starting with Root*)
let depParse lst =
  oracle lst [] [] languageRuleList [] 0

(*Example sentences to be parsed*)
let e1 = [
  Word ("Root",Root);
  Word ("I",N);
  Word ("like",V);
  Word ("the",Det);
  Word ("trees",N);
]

let s1 = [
  Word ("Root", Root);
  Word ("Me",N);
  Word ("gustan",V);
  Word ("los",Det);
  Word ("arboles",N);
]

let p1 = [
  Word ("Root", Root);
  Word ("Eu",N);
  Word ("gosto",V);
  Word ("das",Det);
  Word ("arvores",N);
]

let e2 = [
  Word ("Root", Root);
  Word ("John",N);
  Word ("gave",V);
  Word ("you",N);
  Word ("a",Det);
  Word ("red",Adj);
  Word ("book",N);
]

let e21 = [
  Word ("Root", Root);
  Word ("John",N);
  Word ("gave",V);
  Word ("to",P);
  Word ("you",N);
  Word ("a",Det);
  Word ("red",Adj);
  Word ("book",N);
]

let e23 = [
  Word ("Root", Root);
  Word ("John",N);
  Word ("gave",V);
  Word ("a",Det);
  Word ("red",Adj);
  Word ("book",N);
  Word ("to",P);
  Word ("you",N);
]

let s2 = [
  Word ("Root", Root);
  Word ("John",N);
  Word ("te",N);
  Word ("dio",V);
  Word ("un",Det);
  Word ("libro",N);
  Word ("rojo",Adj);
]

let s23 = [
  Word ("Root",Root);
  Word ("John",N);
  Word ("dio",V);
  Word ("un",Det);
  Word ("libro",N);
  Word ("rojo",Adj);
  Word ("a",P);
  Word ("ti",N);
]

let s22 = [
  Word ("Root",Root);
  Word ("John",N);
  Word ("dio",V);
  Word ("a",P);
  Word ("ti",N);
  Word ("un",Det);
  Word ("libro",N);
  Word ("rojo",Adj);
]

let p2 = [
  Word ("Root",Root);
  Word ("John",N);
  Word ("te",N);
  Word ("deu",V);
  Word ("um",Det);
  Word ("livro",N);
  Word ("vermelho",Adj);
]

let p21 = [
  Word ("Root",Root);
  Word ("John",N);
  Word ("deu",V);
  Word ("te",N);
  Word ("um",Det);
  Word ("livro",N);
  Word ("vermelho",Adj);
]

let p23 = [
  Word ("Root",Root);
  Word ("John",N);
  Word ("deu",V);
  Word ("um",Det);
  Word ("livro",N);
  Word ("vermelho",Adj);
  Word ("para",P);
  Word ("te",N);
]




