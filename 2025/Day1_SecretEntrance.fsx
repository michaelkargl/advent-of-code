printfn "Welcome to Day 1: Secret Entrance!"
printfn "Current Directory: %s" (System.IO.Directory.GetCurrentDirectory())

type Direction =
    | Left
    | Right

type Rotation = { Direction: Direction; Clicks: int }

module Rotation =
    let fromString (s: string) : Rotation =
        let direction =
            match s[0] with
            | 'L' -> Left
            | 'R' -> Right
            | _ -> failwith "Invalid direction"

        let clicks = s[1..] |> int

        { Direction = direction
          Clicks = clicks }

let getRotations (filePath: string) : Rotation seq =
    System.IO.File.ReadAllLines(filePath)
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.filter (fun s -> s.Length > 1)
    |> Seq.map Rotation.fromString

let rotateDial (position: int) (dialSize: int) (rotation: Rotation) : int =
    let clicks = rotation.Clicks % dialSize

    match rotation.Direction with
    // position: 50, clicks: 60, dialSize: 100 = (50 - 60 + 100) % 100 = 90
    | Left -> (position - clicks + dialSize) % dialSize
    | Right -> (position + clicks) % dialSize

let getPasscodeFromRotations (startDialPosition: int) (dialSize: int) (rotations: Rotation seq) : int =
    rotations
    |> Seq.fold
        (fun (currentPosition, passcode) rotation ->
            let newPosition = rotation |> rotateDial currentPosition dialSize
            let newPasscode = if newPosition = 0 then passcode + 1 else passcode
            (newPosition, newPasscode))
        (startDialPosition, 0)
    |> snd

getRotations "inputs/day1.1_rotations.txt"
|> getPasscodeFromRotations 50 100
|> printfn "The passcode is: %d"
