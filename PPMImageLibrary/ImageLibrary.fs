//Name: Adolfo Moreno
//Assignment: Homework 6
//Date 10/15/2015
//Description: Edits Image from GUI

module PPMImageLibrary

#light

//
// DebugOutput:
//
// Outputs to console, which appears in the "Output" window pane of
// Visual Studio when you run with debugging (F5).
//
let rec private OutputImage(image:int list list) = 
  if image = [] then
    printfn "**END**"
  else
    printfn "%A" image.Head
    OutputImage(image.Tail)
           
let DebugOutput(width:int, height:int, depth:int, image:int list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage(image)


//
// TransformFirstRowWhite:
//
// An example transformation: replaces the first row of the given image
// with a row of all white pixels.
//
let rec BuildRowOfWhite cols white = 
  if cols = 0 then
    []
  else 
    // 1 pixel, i.e. RGB value, followed by remaining pixels:
    white :: white :: white :: BuildRowOfWhite (cols-1) white

let FirstRowWhite(width:int, height:int, depth:int, image:int list list) = 
  // first row all white :: followed by rest of original image
  (BuildRowOfWhite width depth) :: image.Tail


let rec ConvertToString (list: int list) = 
  match list with
  | [l] -> l.ToString()
  | head :: tail -> head.ToString() + " " + ConvertToString tail 
  | [] -> ""
    
let Converter (image: int list list) = 
  let convertedimage = List.map (fun x -> (ConvertToString x)) image
  convertedimage

//
// WriteP3Image:
//
// Writes the given image out to a text file, in "P3" format.  Returns true if successful,
// false if not.
//
let rec WriteP3Image(filepath:string, width:int, height:int, depth:int, image:int list list) = 
  //
  // Here's one possible strategy: build a list of strings, then WriteAllLines.
  // Each string appears on a separate line. 
  //
  //let L = ["Hello"; "World"; "1 2 3"; "10 20 30"]
  //let (Header: string)= "P3 \n" + (string width) +" " +(string height) + "\n" + (string depth) + "\n"
  let h= ["P3";string width;string height;string depth]
  let L = Converter image
  let newHeader =  h @ L 

  System.IO.File.WriteAllLines(filepath,newHeader)
  //
  true  // success


//Return average of pixel
let rec getPixelAverage (list:int list) = 
  if list = [] then
  []
  else
  let (R:int) = list.Head //Grab First Element
  let (B:int) = list.Tail.Head //Grab Second Element
  let (G:int) = list.Tail.Tail.Head //Grab Third Element
  let (average: int) = (R+B+G)/3
  let (D: int list) = [average; average; average]
  D @ getPixelAverage list.Tail.Tail.Tail
  
  
let rec Grayscale(width:int, height:int, depth:int, image:int list list) = 
  let n = List.map (fun x -> getPixelAverage x) image
  n

let rec Thresh (list:int list) (threshold:int)= 
  if list = [] then
    []
  else 
    let (A:int) = list.Head
    if A > threshold  then
      255 :: Thresh list.Tail threshold
    else
      0 :: Thresh list.Tail threshold


let rec Threshold(width:int, height:int, depth:int, image:int list list, threshold:int) = 
  let L2 = List.map(fun x -> Thresh x threshold) image
  L2


let rec ReverseThree (list: int list) = 
  if list = [] then 
  []
  else
  let (R: int) = list.Head //Grab First Element
  let (G: int) = list.Tail.Head //Grab Second Element
  let (B: int) = list.Tail.Tail.Head //Grab Third Element
  let (D: int list) = [B;G;R] // Put into a List
  D @ ReverseThree list.Tail.Tail.Tail//Concatenate the tail 

let rec FlipHorizontal(width:int, height:int, depth:int, image:int list list) = 
  let n = List.map (fun x -> ReverseThree x) image
  let z = List.map (fun x -> List.rev x) n
  z
  //
let rec helperOne (list: int list) x = 
  if list = [] then 
    []
  else
    let a = [list.Head; list.Tail.Head;list.Tail.Tail.Head]
    let b = List.replicate x a
    b::helperOne list.Tail.Tail.Tail x
 
let rec Zoom(width:int, height:int, depth:int, image:int list list, factor:int) = 
    let google = List.map (fun x-> helperOne x factor) image
    let test = (List.map (fun x -> List.concat x) google)
    let test2 = (List.map (fun x -> List.concat x) test)
    let test3 = List.map(fun x -> List.replicate factor x) test2
    let final = List.collect(fun x -> x) test3
    final

//Get 3 RGB values
let getThreePix (list: int list) =  
    let (R: int) = list.Head
    let (G: int) = list.Tail.Head
    let (B: int) = list.Tail.Tail.Head
    let (A: int list list) = [[R;G;B]]
    A
  
//Recurse through list in list get me the 3 values and concatenate them to a list
let doHead (list: int list list) = 
  let pixelated = List.collect (fun x -> getThreePix x) list
  pixelated
  

let rec getAverageRecursive (L1: int list list) = 
  if L1 = [] then
    []
  else 
  L1.Head.Head::getAverageRecursive L1.Tail 

let averageHelp (L1: int list list) = 
  let L2 = getAverageRecursive L1
  let L3  = List.average L2
  L3

let rec takeHeadoff (L1: int list list) = 
  if L1 = [] then
    []
  else 
  let L5 = averageHelp L1
 // printfn "%A" L5
  L1.Head.Tail::takeHeadoff L1.Tail 

let rec RotateRight90(width:int, height:int, depth:int, image:int list list) = 
  let L2 = []
  let (m: int list list) = recurseHeads image L2
  m