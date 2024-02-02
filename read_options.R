read_options <- function(txt1, txt2)
{
    #txt1: How many numbers do you want to enter:
    n <- readline(prompt = txt1)
    n <- as.integer(n)
    if (is.na(n)){
        n <- readnumber()
    }
    Numbers<-c()
    for (i in 1:n){
        #Enter an integer:
        num <- readline(prompt = txt2)
        Numbers[i]<-as.numeric(num)
    }
    return(Numbers)
}#print(readnumber())