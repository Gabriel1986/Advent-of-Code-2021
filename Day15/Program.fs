/// https://adventofcode.com/2021/day/15
module Day15
open System
open System.Collections.Generic

type Node =
    {
        I: int
        J: int
        DistanceTraveled: int
        DistanceToGoal: int
    }
    static member Create (i: int, j: int, previousNode: Node option, maze: int[,]) =
        let maxI = maze.GetUpperBound(0)
        let maxJ = maze.GetUpperBound(1)
        {
            I = i
            J = j
            DistanceTraveled = match previousNode with None -> 0 | Some x -> x.DistanceTraveled + maze[i, j]
            DistanceToGoal =  Math.Abs (maxI - i) + Math.Abs (maxJ - j)
        }
    member me.Heuristic = me.DistanceTraveled + me.DistanceToGoal
    member me.IsSameNodeAs (otherNode: Node) = otherNode.I = me.I && otherNode.J = me.J

    member me.Neighbours (maze: int[,]) =
        let maxI = maze.GetUpperBound(0)
        let maxJ = maze.GetUpperBound(1)
        let createNode (i: int, j: int) = Node.Create (i, j, Some me, maze)

        [
            if me.I < maxI then createNode (me.I + 1, me.J)
            if me.J < maxJ then createNode (me.I, me.J + 1)
            if me.I > 0 then createNode (me.I - 1, me.J)
            if me.J > 0 then createNode (me.I, me.J - 1)
        ]

//Had to use mutable collections as using immutable collections made this algorithm too slow :(
let performShortestPathAlgorithm (maze: int[,], startNode: Node, goal: Node) =
    let openNodes = new PriorityQueue<Node, int> ()
    let visitedNodes = new HashSet<int * int> ()
    let mutable minNode = startNode

    while not (minNode.IsSameNodeAs goal) do
        visitedNodes.Add (minNode.I, minNode.J) |> ignore
        for neighbour in minNode.Neighbours maze do
            if (not (visitedNodes.Contains (neighbour.I, neighbour.J))) then
                openNodes.Enqueue(neighbour, neighbour.Heuristic)
        minNode <- openNodes.Dequeue()

    minNode.DistanceTraveled

let private readInput () =
    let inputStrings =
        System.IO.File.ReadLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day15", "Input.txt"))
        |> Seq.toArray

    Array2D.init inputStrings.Length inputStrings[0].Length (fun i j -> Int32.Parse [| inputStrings[i][j] |])

let part1 () =
    let maze = readInput ()
    let maxI = maze.GetUpperBound 0
    let maxJ = maze.GetUpperBound 1

    let shortestPath = performShortestPathAlgorithm (maze, Node.Create (0, 0, None, maze), Node.Create (maxI, maxJ, None, maze))

    printfn "Day 15 - Part 1 - Shortest path weight: %i" shortestPath

let private expand (maze: int[,]): int[,] =
    Array2D.init ((maze.GetUpperBound(0) + 1) * 5) ((maze.GetUpperBound(1) + 1) * 5) (fun i j ->
        let originalI = i % (maze.GetUpperBound(0) + 1)
        let originalJ = j % (maze.GetUpperBound(1) + 1)
        let iterationI = i / (maze.GetUpperBound(0) + 1)
        let iterationJ = j / (maze.GetUpperBound(1) + 1)
        let calculated = maze[originalI, originalJ] + iterationI + iterationJ
        if calculated > 9 then calculated - 9 else calculated
    )

let part2 () =
    let maze = readInput ()
    let expandedMaze = expand maze
    let maxI = expandedMaze.GetUpperBound 0
    let maxJ = expandedMaze.GetUpperBound 1

    let shortestPath = performShortestPathAlgorithm (expandedMaze, Node.Create (0, 0, None, expandedMaze), Node.Create (maxI, maxJ, None, expandedMaze))
    printfn "Day 14 - Part 2 - Shortest path weight: %i" shortestPath