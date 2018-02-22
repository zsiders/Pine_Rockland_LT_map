map.scaleZS <- function (xc, yc, len, units, ndivs, subdiv = 1, tcol = "black", scol = "black", sfcol = "black", tx.cex=1) 
{
    frame = par("usr")
    l <- len
    tic = (frame[4] - frame[3])/100
    ul = l/ndivs

    if(units=="Kilometers"){
        ul2 <- l/ndivs
        ul <- km2ft(ul2)
        l2 <- km2ft(l)
    }else if(units=="Miles"){
        ul2 <- l/ndivs
        ul <- miles2ft(ul2)
        l2 <- miles2ft(l)
    }else if(units=="Meters"){
        ul2 <- l/ndivs
        ul <- km2ft(ul2)/1000
        l2 <- km2ft(l)/1000
    }

    
    for (i in seq(0, ndivs - 1, by = 2)) rect(xc - l2/2 + i * 
        ul, yc, xc - l2/2 + (i + 1) * ul, yc + tic/2, border = NA, 
        col = sfcol)
    lines(c(xc - l2/2, xc - l2/2, xc + l2/2, xc + l2/2), c(yc + tic, 
        yc, yc, yc + tic), col = scol)
    lines(c(xc - l2/2, xc + l2/2), c(yc + tic/2, yc + tic/2), col = scol)
    for (i in 0:ndivs) text(xc - l2/2 + ul * i, yc - strheight(i * 
        subdiv) * 0.7, i * subdiv * ul2, col = tcol, cex=tx.cex)
    text(xc, yc - 2 * strheight(units), units, col = tcol, cex=tx.cex)
}