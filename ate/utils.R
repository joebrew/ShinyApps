#utility functions for randomization, analysis and graphing

#Produce test data with a configurable number of observations and a
#configurable number of predictor variables, each of which has a randomly
#chosen distribution from a set of possible distributions, optionally
#specifying RNG state (seed and generator). The intended use case is
#automated testing of modeling code.
test.data <-
  function(ncols=10, nobs=10000, seed=NULL, kind=NULL)
  {
    if( !is.null(seed) )
    {
      oldseed <- tryCatch(get(".Random.seed", mode="numeric", envir=globalenv()),
                          error=function(x)
                          {
                            runif(1) #seed is unset, generate a new seed
                            .Random.seed
                          })
      set.seed(seed=seed, kind=kind)
      on.exit(assign(".Random.seed", oldseed, envir=globalenv()))
    }
    
    ret <- data.frame(response=as.factor(sample(0:1, nobs, replace=TRUE)),
                      treat=as.factor(sample(0:1, nobs, replace=TRUE)))
    
    #discrete predictors
    #this is ugly - is there a better way to do it?
    unif <- function(y) eval(substitute(expression(sample.int(x, nobs, replace=TRUE)), list(x=y)))
    discrete <- sapply(2:10, unif)
    
    #continuous predictors
    continuous <- c(expression(runif(nobs)),
                    expression(rnorm(nobs)))
    
    #build the columns
    cols <- sample(c(discrete, continuous), ncols, replace=TRUE)
    cols <- lapply(cols, eval.parent)
    cols <- Reduce(cbind, cols)
    
    cols <- as.data.frame(cols)
    names(cols) <- sapply(seq_len(ncols), function(n) paste("predictor.", as.character(n), sep=""))
    
    ret <- cbind(ret, cols)
    structure(ret, predictor.vars=names(cols),
              response.var="response",
              treatment.var="treat")
  }

signif.starred <-
  function(vals, cutpoints=c(0, 0.001, 0.01, 0.1, 1), symbols=c("***", "**", ".", " "))
  {
    stars <- format(symnum(abs(vals), corr=FALSE, na=FALSE, cutpoints=cutpoints, symbols=symbols))
    rounded <- sprintf("%1.3f", vals)
    
    apply(cbind(rounded, stars), 1, paste, collapse=" ")        
  }

resample <-
  function(x, ...) x[sample.int(length(x), ...)]

#take multiple graph objects and put them on a single page,
#with a configurable layout
multiplot <-
  function(..., plotlist=NULL, dims=NULL, file=NULL)
  {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    
    # Find the dimensions of the combined plot.
    if( is.null(dims) )
    {    
      rows <- ceiling(numPlots)
      cols <- 1
    }
    else
    {
      rows <- dims[2]
      cols <- dims[1]
    }
    
    # Make the panel
    layout <- matrix(seq_len(cols * rows), ncol = cols, nrow = rows)
    
    if( !is.null(file) )
    {
      parts <- strsplit(file, "/")[[1]]
      base <- strsplit(parts[length(parts)], "\\.")[[1]]
      if( length(base) == 1 )
        dev <- "jpeg" #pick a default
      else
        dev <- ifelse(base[length(base)] == "jpg", "jpeg", base[length(base)])
      
      if(dev %in% c("pdf", "png", "jpeg", "bmp", "tiff"))
      {
        do.call(dev, list(filename=file))
      }
      else
        stop( paste("unrecognized graphics device: ", dev, sep="") )
    }
    
    if (numPlots==1)
      print(plots[[1]])
    else
    {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots)
      {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
    
    if( !is.null(file) )
      dev.off()
    
    invisible(TRUE)
  }

#creates a random alphanumeric string known not to be in
#a set of other strings. the intended usage is to create
#safe random column names for data frames.
unique.name <-
  function(names=NULL, length=8, alphabet=c(0:9, LETTERS, letters))
  {
    if( is.null(names) )
      return(paste(sample(alphabet, length, replace=TRUE), collapse=""))
    else
      while(TRUE) #no upper bound on iterations, but the loop halts with probability 1
      {
        name <- paste(sample(alphabet, length, replace=TRUE), collapse="")
        if( name %in% names )
          next
        else
          return(name)
      }
  }

#expands treatment conditions for factorialized experiments
#into a list of value labels
make.labels <- 
  function(..., sep=" : ")
  {
    vars <- list(...)
    x <- do.call(expand.grid, vars)
    
    do.call(paste, c(x, sep=sep))
  }

#because the *apply functions are scary for newbies
misschk <-
  function(data, all=TRUE, cols=NULL)
  {
    if( is.null(cols) )
      c <- names(data)
    else
      c <- cols
    
    if( !is.null(all) && all )
      f <- function(x) length(which(is.na(x) | is.null(x) | x == ""))
    else
      f <- function(x) length(which(is.na(x)))
    
    sapply(data[,c], f)
  }

household <-
  function (data, hhvars, hh.rep = "hh_rep", hh.id = "hh_id", hh.size = "hh_size",
            drop.nonreps = TRUE, drop.large.hh = 5, stats = FALSE)
  {
    if (is.list(hhvars))
      hhvars <- unlist(hhvars)
    
    #calculate the hh id
    raw <- do.call(paste, lapply(data[, hhvars], as.character))
    tmp <- match(raw, unique(raw))
    
    if (!is.null(hh.id))
      data[, hh.id] <- tmp
    
    tmp <- as.data.frame(tmp)
    names(tmp)[1] <- hh.id
    
    if (!is.null(hh.size) || (!is.null(drop.large.hh) && drop.large.hh))
    {
      cnt <- as.data.frame(tabulate(tmp[[hh.id]], nbins = max(tmp[[hh.id]])))
      names(cnt)[1] <- "hh.size"
      cnt[, hh.id] <- seq_len(nrow(cnt))
      
      tmp <- merge(tmp, cnt, by = hh.id)
      data <- data[order(data[, hh.id]), ]
      tmp <- tmp[order(tmp[, hh.id]), ]
      
      #if requested, include hh id variable
      if (!is.null(hh.size))
        data[, hh.size] <- tmp$hh.size
      
      if (!is.null(stats) && stats)
      {
        print(summary(tmp$hh.size))
        print(table(tmp$hh.size))
      }
      
      #drop large households if requested
      if (!is.null(drop.large.hh) && drop.large.hh)
        data <- data[which(tmp[, "hh.size"] < drop.large.hh),]
    }
    
    #if requested, calculate hh reps and drop non-reps
    if (!is.null(hh.rep) || (!is.null(drop.nonreps) && drop.nonreps))
    {
      tmp <- tmp[order(runif(nrow(tmp))), ]
      tmp$rn <- ave(tmp[, hh.id], tmp[, hh.id], FUN = seq_along)
      tmp <- tmp[order(tmp[, hh.id]), ]
      tmp[, hh.rep] <- as.numeric(tmp$rn == 1)
      
      if (!is.null(hh.rep))
        data[[hh.rep]] <- tmp[[hh.rep]]
      
      if (!is.null(drop.nonreps) && drop.nonreps)
        data <- data[which(tmp[, hh.rep] == 1), ]
    }
    
    return(data)
  }
