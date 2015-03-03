// Weitere Informationen zu F# finden Sie unter http://fsharp.net. Im Projekt 'F#-Lernprogramm' finden Sie
// einen Leitfaden zum Programmieren in F#.
//#load "Library1.fs"
//open Yaaf.Xmpp.IM.ArchiveManagement
(*
// TODO: report format bug
[<System.Serializable>]
type AException =     
    inherit System.Exception
    new (msg : string) = { inherit System.Exception(msg) }
    new (msg:string, inner:System.Exception) = { inherit System.Exception(msg, inner) }       
    new (info:System.Runtime.Serialization.SerializationInfo, context:System.Runtime.Serialization.StreamingContext) = {
        inherit System.Exception(info, context)
    }
    
[<System.Serializable>]
type BException =
    inherit AException
    new (msg : string) = { inherit AException(msg) }

// TODO: report format bug
let t = true
let r = false
let a = 
    lazy 
        t && r

// TODO: report format bug: FS0058
type R =
    | A
    | B

type C =
    abstract member D : int with get
let r = A
let c = 
    { new C with
        member x.D
            with get() =
                match r with
                | A -> 1
                | B -> 2 }
             
    
// TODO: report format bug: comments get moved somewhere?
// Note the definition for !?> is not even required to trigger the bug
module Test =
    type ushort = System.UInt16
    //let inline (!>) (arg:^b) : ^a = (^b : (static member op_Implicit: ^b -> ^a) arg)
    //let inline (!?>) (arg:^b) : ^a = (^b : (static member op_Explicit: ^b -> ^a) arg)
    let inline ushort s = (!?> s) : ushort
    //module Helpers = 
    let DecodeIntegerSize () =
        let mutable byteValue = 0uy
        if (byteValue <> 0x02uy) then     // indicates an ASN.1 integer value follows
            0
        else
        4
        // anothercomment

// TODO: report format bug: invalid position of ".Data"
type F = 
    | Test of string * string
    member x.Data =
        match x with
        | Test(first,secound) -> first
let s : string =
    Test(
        "Some very long line                        asdasd                        asdasdas                 asdasd ",
        "Again some long line                                    ubhgsafd                        asdofb                ashdbf")
        .Data

        *)