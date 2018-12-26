

let changeStr (a:string):int list=
    let mutable list=[]
    for i=1 to a.Length-2 do
        let c=(int)a.[i]
        list<- c :: list
        printf "%d" c
    list
let c=changeStr("123")

printf "%d" c.[0]