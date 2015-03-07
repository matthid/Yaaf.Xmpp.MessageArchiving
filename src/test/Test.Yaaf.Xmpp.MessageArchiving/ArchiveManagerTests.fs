// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.MessageArchiveManager

open FsUnit
open NUnit.Framework
open Yaaf.Helper
open Yaaf.TestHelper

open Test.Yaaf.Xmpp
open Yaaf.Xmpp
open Yaaf.Xmpp.IM.Server
open Yaaf.Xmpp.IM
open Swensen.Unquote
open Foq
open Yaaf.Xmpp.MessageArchiving

[<TestFixture>]
type ``Yaaf-Xmpp-MessageArchiveManager: Some interface tests``() =
    inherit MyTestClass()

    let mutable backend = Unchecked.defaultof<_>
    let defUser = (JabberId.Parse "user@nunit.org")
    abstract CreateBackend : unit -> IMessageArchivingStore
    default x.CreateBackend () =
        MemoryMessageArchivingStore () :> IMessageArchivingStore

    override x.Setup () = 
        backend <- x.CreateBackend()
        base.Setup()

    override x.TearDown() = 
        backend <- Unchecked.defaultof<_>
        base.TearDown()
        
        
    [<Test>]
    member x.``Check that storing and deleting default preferences works``() =
        let prefStore = backend.GetPreferenceStore defUser |> waitTask
        test <@ prefStore.GetUserPreferences () |> waitTask = None @>

        prefStore.SetDefaultOtrSaveMode (Some { Expire = Some 123L; OtrMode = Some OtrMode.Concede; SaveMode = Some SaveMode.Body }) |> Task.ofPlainTask |> waitTask
        let results = prefStore.GetUserPreferences () |> waitTask
        test <@ results.IsSome @>
        let saveMode = results.Value.DefaultOtrSaveMode;
        test <@ saveMode.Expire = Some 123L @>
        test <@ saveMode.OtrMode = Some OtrMode.Concede @>
        test <@ saveMode.SaveMode = Some SaveMode.Body @>
        prefStore.SetDefaultOtrSaveMode None |> Task.ofPlainTask |> waitTask

        let results = prefStore.GetUserPreferences () |> waitTask
        test <@ results.IsNone @>