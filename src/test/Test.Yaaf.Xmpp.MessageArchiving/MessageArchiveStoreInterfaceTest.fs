// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.IM

open FsUnit
open NUnit.Framework
open Test.Yaaf.Xmpp
open Yaaf.Xmpp
open Yaaf.Xmpp.IM
open Yaaf.Helper
open Yaaf.TestHelper
open Yaaf.Xmpp.MessageArchiving
open Yaaf.Xmpp.IM.Server
open Swensen.Unquote

/// This base class provides some basic unit tests which should work on all instances of MessageArchivingStore
/// So everyone can just inherit from this class and use those tests for their own implementation
[<TestFixture>]
type MessageArchivingStoreTest () = 
    inherit MyTestClass()

    let mutable archivingStore = Unchecked.defaultof<_>

    static let toMessageSimple (isTo,msg) = 
        let msgInfo = {
                Sec = None
                Utc = Some System.DateTime.Now
                Name = None
                Jid = None
            }
        let content = {
                Body = Some msg
                AdditionalData = []
            }
        if isTo then
            ChatItem.To(msgInfo, content)
        else
            ChatItem.From(msgInfo, content)
                
    static member CreateMessage (isTo,msg) = toMessageSimple (isTo,msg)
    abstract member CreateArchivingStore : unit -> (IMessageArchivingStore)
    default x.CreateArchivingStore () = new MemoryMessageArchivingStore() :> IMessageArchivingStore
    
    override x.Setup () = 
        // Setup DataDirectory for databases (if any)
        System.AppDomain.CurrentDomain.SetData(
            "DataDirectory", 
            System.AppDomain.CurrentDomain.BaseDirectory)

        archivingStore <- x.CreateArchivingStore()
        base.Setup()

    override x.TearDown() = 
        match archivingStore with
        | :? System.IDisposable as d -> d.Dispose()
        | _ -> ()
        archivingStore <- Unchecked.defaultof<_>
        base.TearDown()
                

    [<Test>]
    member x.``Check that saving a collection works`` () = 
        let userStore = archivingStore.GetArchiveStore (JabberId.Parse "test@nunit.org") |> waitTask

        let collection =
            {
                Header = 
                    {
                        Id = 
                            {
                                With = JabberId.Parse "other@nunit.org"
                                Start = StreamData.DateTime.Parse "2010-07-21T02:56:15Z" 
                            }
                        Thread = Some "thread"
                        Version = None
                        Subject = Some "subject"
                    }
                ChatItems = 
                    seq {
                        yield false, "firstmessage"
                        yield true, "returnmessage"
                    }
                    |> Seq.map toMessageSimple
                    |> Seq.toList
                SetPrevious = false
                Previous = None
                SetNext = false
                Next = None
            }

        userStore.StoreCollection collection |> Task.ofPlainTask |> waitTask
        let retrieved = userStore.RetrieveCollection collection.Header.Id |> waitTask
        // Version has to be set
        retrieved.Header.Version.IsSome |> should be True
        
        let retrieved = userStore.FilterMessages CollectionFilter.Any |> waitTask
        retrieved.Length |> should be (equal 1)
         

    [<Test>]
    member x.``Check that deleting a collection works`` () = 
        let userStore = archivingStore.GetArchiveStore (JabberId.Parse "test@nunit.org") |> waitTask

        let collection =
            {
                Header = 
                    {
                        Id = 
                            {
                                With = JabberId.Parse "other@nunit.org"
                                Start = StreamData.DateTime.Parse "2010-07-21T02:56:15Z" 
                            }
                        Thread = Some "thread"
                        Version = None
                        Subject = Some "subject"
                    }
                ChatItems = 
                    seq {
                        yield false, "firstmessage"
                        yield true, "returnmessage"
                    }
                    |> Seq.map toMessageSimple
                    |> Seq.toList
                SetPrevious = false
                Previous = None
                SetNext = false
                Next = None
            }

        userStore.StoreCollection collection |> Task.ofPlainTask |> waitTask
        test <@ userStore.RemoveCollection collection.Header.Id |> waitTask @>
        test <@ userStore.RemoveCollection collection.Header.Id |> waitTask |> not @>
        raises<System.Collections.Generic.KeyNotFoundException> <@ userStore.RetrieveCollection collection.Header.Id |> waitTask @>

        let retrieved = userStore.FilterMessages CollectionFilter.Any |> waitTask
        retrieved.Length |> should be (equal 0)
        
    [<Test>]
    member x.``Check that editing a collection works`` () = 
        let userStore = archivingStore.GetArchiveStore (JabberId.Parse "test@nunit.org") |> waitTask
        let collection =
            {
                Header = 
                    {
                        Id = 
                            {
                                With = JabberId.Parse "other@nunit.org"
                                Start = StreamData.DateTime.Parse "2010-07-21T02:56:15Z" 
                            }
                        Thread = Some "thread"
                        Version = None
                        Subject = Some "subject"
                    }
                ChatItems = 
                    seq {
                        yield false, "firstmessage"
                        yield true, "returnmessage"
                    }
                    |> Seq.map toMessageSimple
                    |> Seq.toList
                SetPrevious = false
                Previous = None
                SetNext = false
                Next = None
            }

        userStore.StoreCollection collection |> Task.ofPlainTask |> waitTask
        let retrieved = userStore.RetrieveCollection collection.Header.Id |> waitTask
        let oldVersion = retrieved.Header.Version.Value
        let newCol = { collection with Header = { collection.Header with Subject = Some "newsubject" }}
        userStore.StoreCollection newCol |> Task.ofPlainTask |> waitTask
        let retrieved = userStore.RetrieveCollection collection.Header.Id |> waitTask
        // Version has to be set
        retrieved.Header.Version.IsSome |> should be True
        // new version!
        retrieved.Header.Version.Value > oldVersion |> should be True
        retrieved.Header.Subject.Value  |> should be (equal "newsubject")
        // We edited, so there should be still 1
        let retrieved = userStore.FilterMessages CollectionFilter.Any |> waitTask
        retrieved.Length |> should be (equal 1)
        
    [<Test>]
    member x.``Check that editing a collection works with different id object instance`` () = 
        let userStore = archivingStore.GetArchiveStore (JabberId.Parse "test@nunit.org") |> waitTask
        let collection =
            {
                Header = 
                    {
                        Id = 
                            {
                                With = JabberId.Parse "other@nunit.org"
                                Start = StreamData.DateTime.Parse "2010-07-21T02:56:15Z" 
                            }
                        Thread = Some "thread"
                        Version = None
                        Subject = Some "subject"
                    }
                ChatItems = 
                    seq {
                        yield false, "firstmessage"
                        yield true, "returnmessage"
                    }
                    |> Seq.map toMessageSimple
                    |> Seq.toList
                SetPrevious = false
                Previous = None
                SetNext = false
                Next = None
            }
        let id =
            {
                With = JabberId.Parse "other@nunit.org"
                Start = StreamData.DateTime.Parse "2010-07-21T02:56:15Z" 
            } : ChatCollectionId
        userStore.StoreCollection collection |> Task.ofPlainTask |> waitTask
        let retrieved = userStore.RetrieveCollection id |> waitTask
        let oldVersion = retrieved.Header.Version.Value
        let newCol = { collection with Header = { collection.Header with Subject = Some "newsubject"; Id = id }}
        userStore.StoreCollection newCol |> Task.ofPlainTask |> waitTask
        let retrieved = userStore.RetrieveCollection id |> waitTask
        // Version has to be set
        retrieved.Header.Version.IsSome |> should be True
        // new version!
        retrieved.Header.Version.Value > oldVersion |> should be True
        retrieved.Header.Subject.Value  |> should be (equal "newsubject")
        // We edited, so there should be still 1
        let retrieved = userStore.FilterMessages CollectionFilter.Any |> waitTask
        retrieved.Length |> should be (equal 1)
    
    [<Test>]
    member x.``Check that changes are correctly recorded (slow)`` () = 
        let userStore = archivingStore.GetArchiveStore (JabberId.Parse "test@nunit.org") |> waitTask
        let collection =
            {
                Header = 
                    {
                        Id = 
                            {
                                With = JabberId.Parse "other@nunit.org"
                                Start = StreamData.DateTime.Parse "2010-07-21T02:56:15Z" 
                            }
                        Thread = Some "thread"
                        Version = None
                        Subject = Some "subject"
                    }
                ChatItems = 
                    seq {
                        yield false, "firstmessage"
                        yield true, "returnmessage"
                    }
                    |> Seq.map toMessageSimple
                    |> Seq.toList
                SetPrevious = false
                Previous = None
                SetNext = false
                Next = None
            }
        let id =
            {
                With = JabberId.Parse "other@nunit.org"
                Start = StreamData.DateTime.Parse "2010-07-21T02:56:15Z" 
            } : ChatCollectionId
        let empty = System.DateTime.Now
        userStore.StoreCollection collection |> Task.ofPlainTask |> waitTask
        System.Threading.Thread.Sleep 1000
        let changes = userStore.GetChangesSince empty |> waitTask
        changes.Length |> should be (equal 1)
        let first = System.DateTime.Now
        let newCol = { collection with Header = { collection.Header with Subject = Some "newsubject"; Id = id }}
        userStore.StoreCollection newCol |> Task.ofPlainTask |> waitTask
        System.Threading.Thread.Sleep 1000
        let secound = System.DateTime.Now

        let changes = userStore.GetChangesSince empty |> waitTask
        changes.Length |> should be (equal 1)
        let changes = userStore.GetChangesSince first |> waitTask
        changes.Length |> should be (equal 1)
        
        userStore.RemoveCollection newCol.Header.Id |> waitTask |> should be True
        
        let changes = userStore.GetChangesSince empty |> waitTask
        if changes.Length <> 0 then
            changes.Length |> should be (equal 1)

        let changes = userStore.GetChangesSince secound |> waitTask
        changes.Length |> should be (equal 1)

    [<Test>]
    member x.``Check that changes are correctly recorded`` () = 
        let userStore = archivingStore.GetArchiveStore (JabberId.Parse "test@nunit.org") |> waitTask
        let collection =
            {
                Header = 
                    {
                        Id = 
                            {
                                With = JabberId.Parse "other@nunit.org"
                                Start = StreamData.DateTime.Parse "2010-07-21T02:56:15Z" 
                            }
                        Thread = Some "thread"
                        Version = None
                        Subject = Some "subject"
                    }
                ChatItems = 
                    seq {
                        yield false, "firstmessage"
                        yield true, "returnmessage"
                    }
                    |> Seq.map toMessageSimple
                    |> Seq.toList
                SetPrevious = false
                Previous = None
                SetNext = false
                Next = None
            }
        let id =
            {
                With = JabberId.Parse "other@nunit.org"
                Start = StreamData.DateTime.Parse "2010-07-21T02:56:15Z" 
            } : ChatCollectionId
        let empty = System.DateTime.Now
        userStore.StoreCollection collection |> Task.ofPlainTask |> waitTask
        let changes = userStore.GetChangesSince empty |> waitTask
        changes.Length |> should be (equal 1)
        let first = System.DateTime.Now
        let newCol = { collection with Header = { collection.Header with Subject = Some "newsubject"; Id = id }}
        userStore.StoreCollection newCol |> Task.ofPlainTask |> waitTask
        let secound = System.DateTime.Now

        let changes = userStore.GetChangesSince empty |> waitTask
        changes.Length |> should be (equal 1)
        let changes = userStore.GetChangesSince first |> waitTask
        changes.Length |> should be (equal 1)
        
        userStore.RemoveCollection newCol.Header.Id |> waitTask |> should be True
        
        let changes = userStore.GetChangesSince empty |> waitTask
        if changes.Length <> 0 then
            changes.Length |> should be (equal 1)

        let changes = userStore.GetChangesSince secound |> waitTask
        changes.Length |> should be (equal 1)


