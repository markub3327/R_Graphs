library(igraph)

g = make_lattice(length = 4, dim = 2)
myLayout = layout_on_grid(g)
plot(g, layout=myLayout)
as_edgelist(g, names = TRUE)
g = delete.edges(g, c(6, 8, 10, 11, 12, 13, 18, 19))           
plot(g, layout=myLayout)


myBFS = bfs(g, root=11, order=TRUE, rank=TRUE, father=TRUE, pred=TRUE, succ=TRUE, dist=TRUE)
myBFS

myDFS = dfs(g, root=11, unreachable=TRUE, order=TRUE, father=TRUE, dist=TRUE)
myDFS

for (i in 1:vcount(g))
{
  goal = i
  myPath1 = c(goal)
  myROOT = 11
  repeat {
    goal = myBFS$father[goal]
    myPath1 = c(myPath1, goal)
    if (goal == myROOT) break
  }


  goal = i
  myPath2 = c(goal)
  myROOT = 11
  repeat {
    goal = myDFS$father[goal]
    myPath2 = c(myPath2, goal)
    if (goal == myROOT) break
  }

  if (identical(myPath1, myPath2) == FALSE)
  {
    cat(rev(myPath1), "\n")
    cat(rev(myPath2), "\n")
    cat(i, "\n")
  }
}
