

let changeHEX (a:string):int=
    let mutable len=a.Length
    let mutable flag=0
    if a.[0]='-' then
      flag<-1
      printf "%c"a.[0]
    else flag<-0
   
    let mutable sum=0
    for j=flag to len-1 do
      let d=(int)a.[j]-48
      let mutable x=0
      if (int)a.[j]<=57 then
        x <-(int)a.[j]-48
      else 
        x<-(int)a.[j]-65+10
      printf"%d" d
      sum <- sum+(int)(16.0**(float)(len-j-1))*x
    if flag=1 then
      sum<- -sum

    sum


let c=changeHEX("-FF")
