open System
open System.Collections
open System.ComponentModel

// let data =([|1uy; 1uy; 1uy; 1uy|]); 
// let f = BitConverter.ToSingle(data, 0);
 
let c=Convert.ToByte(123.3)

let aSingl  = 2.1000000f;

let q=BitConverter.ToInt32(BitConverter.GetBytes( aSingl ),0)
let d=BitConverter.ToString( BitConverter.GetBytes( aSingl ) )

let h=(int)((string)(d.[9..10]+d.[6..7]+d.[3..4]+d.[0..1]))

let a=0xFF