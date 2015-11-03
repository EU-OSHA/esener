
documentSession <- function(location = ""){
	#documents the objects of the current session in functions.txt and variables.txt
	sessionObjects <- ls(envir = parent.frame())
	

	functions <- c()
	variables <- c()
	functions.in.package <- c()

	for(index in 1:length(sessionObjects)){
	
		object.name <- sessionObjects[index]
		object.class <- interpret(paste("class(", object.name ,")"))[1]

			if(object.class == "function") {
				function.in.package <- grep("documentationGenerate",object.name)
			
				if ( (length(function.in.package) > 0 ) || (object.name == "interpret") || (object.name == "writeStringToFile") || (object.name == "documentSession" ) )
				   functions.in.package <- c(functions.in.package,object.name)
				else {
					function.documentation <- documentationGenerateFunction( object.name )
					functions <- c(functions, function.documentation)
				}
			}else{
				variable.documentation <- ""

				if ( object.class == "data.frame" ){
					variable.documentation <- documentationGenerateDataFrame( object.name )
				}
				else if ( object.class == "numeric"){
					if( length(get(object.name)) <= 1){
						variable.documentation <- documentationGenerateNumeric( object.name )
					}
					else if( length(get(object.name)) > 1 ){
						variable.documentation <- documentationGenerateVector( object.name )
					}
				}
				else if( object.class == "character"){
						variable.documentation <- documentationGenerateCharacter( object.name )
				}
				else if( object.class == "logical"){
					variable.documentation <- documentationGenerateLogical( object.name )
				}
				else if( object.class == "complex"){
					variable.documentation <- documentationGenerateComplex( object.name )
				}
				else if ( object.class == "factor" ){
					variable.documentation <- documentationGenerateFactor( object.name )
				}
				else if( object.class == "integer"){
					variable.documentation <- documentationGenerateInteger( object.name )
				}
				else if( object.class == "NULL"){
					variable.documentation <- documentationGenerateNull( object.name )
				}
				else if( object.class == "matrix" ){
					variable.documentation <- documentationGenerateMatrix( object.name )
				}
				else {
					print(paste("unknown class my friend:", object.class))
				}
				variables <- c(variables, variable.documentation)
			}
	}

	if(length(functions) > 0) writeStringToFile(functions, paste(location, "functions.txt", sep = ""))
	if(length(variables) > 0) writeStringToFile(variables, paste(location, "variables.txt", sep = ""))


	return(functions.in.package)
}


documentationGenerateDataFrame <- function( object.name ){
	#prints the columns of the data frame (if more than 10, the first 10) and the values of each column (if more than 10 the first 10
		documentation.rows <- c()

	maximum.values <- c()
	values <- c()

	if(  (length( names(get(object.name) ) ) ) > 10 ) 	maximum.columns <- 10    #maximum number of columns of a data.frame shown
	else maximum.columns <-  (length( names(get(object.name) ) ) )

	columns.names <- ""
	for ( index.columns in 1:maximum.columns ) {
		columns.names <- paste(columns.names,names(get(object.name) )[index.columns],sep=" ")		#there are more columns not shown
		if(  length(  (get(object.name) )[,index.columns] ) > 10  )		maximum.values[index.columns] <- 10 	#maximum number of values of each column shown
		else maximum.values[index.columns] <-  length(  (get(object.name) )[,index.columns] ) 
		values[index.columns] <- ""
		for( index.value in 1:maximum.values[index.columns] ){
			values[index.columns] <- paste(values[index.columns],get(object.name)[index.value,index.columns])
		}
		if( length(  (get(object.name) )[,index.columns]  ) > 10 )   values[index.columns] <- paste(values[index.columns]," ..... ")	#there are more values not shown
	}
	if( (length( names(get(object.name) ) ) ) > 10 )	columns.names <- paste(columns.names," ..... ",sep=" ")

	documentation.rows <- c(documentation.rows , object.name  )
	documentation.rows <- c(documentation.rows , "      data frame"  )
	documentation.rows <- c(documentation.rows , (paste("            columns names: ",columns.names) )  )
	for( index.columns in 1:maximum.columns ){
		documentation.rows <- c(documentation.rows , (paste("            values of('",names(get(object.name) )[index.columns],"'): ",values[index.columns]) ) )
	}

	return (documentation.rows)

}

documentationGenerateMatrix <- function( object.name ){
	documentation.rows <- c()

	if( nrow(get(object.name)) > 10 )	maximum.x <- 10
	else maximum.x <- nrow(get(object.name))

	if( ncol(get(object.name)) > 10 )	maximum.y <- 10
	else maximum.y <- ncol(get(object.name))


	documentation.rows <- c(documentation.rows , object.name )
	documentation.rows <- c(documentation.rows , "      matrix" )
	documentation.rows <- c(documentation.rows , "            values: ")
	for(i in 1:maximum.x){
		each.row <- "            "
		for(j in 1:maximum.y){
			each.row <- paste(each.row,as.character(get(object.name)[i,j]),sep = " ")
		}
		if( ncol(get(object.name)) > 10 ) each.row <- paste(each.row," ....")
		documentation.rows <- c(documentation.rows,each.row)
	}

	if( nrow(get(object.name)) > 10 )  documentation.rows <- c(documentation.rows,"       .......... ")

	return(documentation.rows)
}

documentationGenerateFactor <- function (object.name){
	#prints the values and the levels of a factor (if more than 10, the first 10)
	documentation.rows <- c()

	object.values <- interpret(object.name)
	object.levels <- levels(get(object.name))

	if(length(object.values) > 10 )		maximum.values <- 10
	else	maximum.values <- length(object.values)

	if( length(object.levels) > 10 )	maximum.levels <- 10
	else maximum.levels <- length(object.levels)

	values <- ""
	for(index in 1:maximum.values){
		values <- paste(values,object.values[index])
	}
	if(length(object.values) > 10 )		values <- paste(values," ..... ")

	levels <- ""
	for(index in 1:maximum.levels){
		levels <- paste(levels,object.levels[index])
	}
	if(length(object.levels) > 10 )		levels <- paste(levels," ..... ")

	documentation.rows <- c(documentation.rows, object.name)
	documentation.rows <- c(documentation.rows, "      factor")
	documentation.rows <- c(documentation.rows , (paste("            values :",values)) )
	documentation.rows <- c(documentation.rows , (paste("            levels :",levels)) )

	return (documentation.rows)
}

documentationGenerateComplex <- function( object.name ){
	documentation.rows <- c()

	object.Real <- interpret(Re( get(object.name) ))
	object.Imaginary <- interpret(Im( get(object.name) ))

	documentation.rows <- c(documentation.rows , object.name)
	documentation.rows <- c(documentation.rows , "      complex")
	documentation.rows <- c(documentation.rows , (paste("            Real Part:",object.Real," , Imaginary Part:",object.Imaginary)) )

	return (documentation.rows)
}

documentationGenerateLogical <- function( object.name ){
	documentation.rows <- c()
	object.value <- interpret(object.name)

	documentation.rows <- c(documentation.rows , object.name)
	documentation.rows <- c(documentation.rows , "      logical")
	documentation.rows <- c(documentation.rows , (paste("            value:",object.value)) )

	return (documentation.rows)
}

documentationGenerateNumeric <- function ( object.name ){
	documentation.rows <- c()

	documentation.rows <- c(documentation.rows , object.name)
	documentation.rows <- c(documentation.rows , "      numeric")
	if( length(get(object.name)) == 0){
		documentation.rows <- c(documentation.rows , "            value:  empty (length = 0)" )
	}
	else{
		object.value <- interpret(object.name)
		documentation.rows <- c(documentation.rows , (paste("            value:",object.value,sep = "")) )
	}
	return (documentation.rows)
}

documentationGenerateInteger <- function(object.name){
	documentation.rows <- c()

	object.value <- interpret(object.name)
	documentation.rows <- c(documentation.rows , object.name)
	documentation.rows <- c(documentation.rows , "      integer")
	if(length(object.name > 0))  documentation.rows <- c(documentation.rows , (paste("            value:",object.value,sep = "")) )
	else   documentation.rows <- c(documentation.rows ,  "value: 0"  )
	return (documentation.rows)

}

documentationGenerateCharacter <- function( object.name ){
	documentation.rows <- c()

	object.value <- interpret(object.name)
	documentation.rows <- c(documentation.rows , object.name )
	documentation.rows <- c(documentation.rows , "      character" )
	documentation.rows <- c(documentation.rows , (paste("            value:",object.value,sep = "")  ) )

	return (documentation.rows)
}

documentationGenerateNull <- function (object.name){
	documentation.rows <- c()

	documentation.rows <- c(documentation.rows , object.name )
	documentation.rows <- c(documentation.rows , "      NULL" )
	return (documentation.rows)
}

documentationGenerateVector <- function( object.name ){
	#prints the values of the vector (if more than 10,the first 10)
	documentation.rows <- c()

	vector.class <- interpret(paste("class(", object.name ,")"))[1]

	if( length( get( object.name ) ) > 10 ) maximum.values <- 10
	else maximum.values <- length( get( object.name ) )

	values <- ""
	for( index in 1:maximum.values ){
		values <- paste( values , get(object.name)[index] )
	}
	if( length( get( object.name ) ) > 10 )  values <- paste( values , " ..... ")	#there are more values not shown
	documentation.rows <- c(documentation.rows , object.name)
	documentation.rows <- c(documentation.rows , "      vector")
	documentation.rows <- c(documentation.rows , ( paste("            values :",values)) )

	return (documentation.rows)
}

writeStringToFile <- function(string, fileLocation){

	fileReference <- file(fileLocation)
	writeLines(string, fileReference )
	close(fileReference)
}


documentationGenerateFunction <- function( object.name ){
	#prints the name of the function and its arguments and the "documentation comments" below (if there are)

  	object.string <- interpret(object.name)
  	function.source.code <- deparse(object.string,control = "useSource")

	documentation.rows <- c()

	source.in.string <- ""
	for(index in 1:length(function.source.code) ){
		if(index >1 )  source.in.string <- paste(source.in.string,function.source.code[index],sep="\n")
		else if( index == 1)  source.in.string <- paste(source.in.string,function.source.code[index],sep="")
	}

	function.arguments <- sub("[{]((.)*[\t\n ]*)*","",source.in.string)
	function.arguments <-  sub("function","",function.arguments)
	function.arguments <- sub("[(]","",function.arguments)
	function.arguments <- sub("[])]","",function.arguments)
	documentation.rows <- c(documentation.rows , object.name )
	documentation.rows <- c(documentation.rows , paste("      ",function.arguments) )

	x <- c(source.in.string)
	comments.exist <- grep("[{]([ \t\n])*(#[^\n]*(\n))+", x, perl=TRUE)
	m <- regexpr("[{]([ \t\n])*(#[^\n]*(\n))*", x, perl=TRUE)
	function.comments <- regmatches(x, m)
	function.comments <- sub("[{]([ \t\n])*","",function.comments)
	function.comments <- gsub("[\n]","",function.comments)
	comments.vector <- unlist( strsplit(function.comments,"#") )
	comments.vector[2] <- paste("      ",comments.vector[2])
	comments.vector <- comments.vector[2:length( comments.vector )]

	if( length(comments.exist)>0 )		documentation.rows <- c(documentation.rows,comments.vector,"")

	return(documentation.rows)
}


interpret <- function(code){
	return(eval(parse(text = code)))
}


rm(list = documentSession( "" ) )
	
# source("documentSession.R")