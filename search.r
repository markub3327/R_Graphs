library("igraph")

do_sirky <- function(graph, root) {
    D <- c(as_ids(V(g)[root]))
    mojLayout <- layout_nicely(graph, dim=2)
    plot(graph, layout=mojLayout)

    V(g)$zafarbeny <- FALSE

    while (length(D)) {
        if (V(g)[D[1]]$zafarbeny) {
            D <- D[-1]
        }
        else {
            print(D[1])

            # zafarbi navstiveny vrchol
            V(g)[D[1]]$zafarbeny <- TRUE

            # vymaz pouzity vrchol a uloz susedov
            susedia <- neighbors(graph, D[1])
            D <- D[-1]
            D <- append(D, as_ids(susedia), after = length(D))
            plot(g, vertex.color=V(g)$zafarbeny, layout=mojLayout, main=paste("Vrcholy", D, sep=" "))

            invisible(readline(prompt="Press [enter] to continue"))
        }
    }
}

do_hlbky <- function(graph, root) {
    D <- c(as_ids(V(g)[root]))
    mojLayout <- layout_nicely(graph, dim=2)
    plot(graph, layout=mojLayout)

    V(g)$zafarbeny <- FALSE

    while (length(D)) {
        if (V(g)[D[1]]$zafarbeny) {
            D <- D[-1]
        }
        else {
            print(D[1])

            # zafarbi navstiveny vrchol
            V(g)[D[1]]$zafarbeny <- TRUE

            # vymaz pouzity vrchol a uloz susedov
            susedia <- neighbors(graph, D[1])
            D <- D[-1]
            D <- append(D, as_ids(susedia), after = 0)
            plot(g, vertex.color=V(g)$zafarbeny, layout=mojLayout, main=paste("Vrcholy", D, sep=" "))

            invisible(readline(prompt="Press [enter] to continue"))
        }
    }
}

g <- make_graph(c(1,2, 1,3, 1,4, 2,7, 2,14, 3,5, 4,8, 7,12, 7,15, 5,6, 5,13, 8,9, 8,10, 8,11))
do_sirky(g, 1)
do_hlbky(g, 1)
