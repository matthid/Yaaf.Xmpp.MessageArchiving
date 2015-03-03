// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiving

open Yaaf.Helper

open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.IM

open Yaaf.Logging
open Yaaf.Xmpp.ServiceDiscovery
open Yaaf.Xmpp.MessageArchiving

type IMessageArchivingClientPluginService =
    

    abstract PrefRequest : unit -> Task<PreferenceInfo>
    abstract PrefSetItem : OtrItem -> unit
    abstract PrefSetSession : SessionItem -> unit
    
    abstract PrefSetMethods : AllMethodSettings -> unit
    abstract PrefSetDefault : OtrSaveMode -> unit
    abstract PrefRemoveItem :JabberId -> unit
    abstract PrefRemoveSession : ThreadId -> unit
    // Manual
    abstract SaveCollection : ChatCollection -> Task<ChatCollectionHeader>
    // Automatic
    abstract SetAutomaticSave : bool * (ArchivingScope option) -> Task<unit>
    // Replication
    abstract RequestModifiedSince : System.DateTime -> Task<ChangeItem list>
    // Management
    abstract RequestCollectionList : CollectionFilter -> Task<ChatCollectionHeader list>
    abstract RequestCollection : ChatCollectionId -> Task<ChatCollection>
    abstract RemoveCollection : ChatCollectionId -> Task<unit>
    abstract RemoveAllCollections : CollectionFilter -> Task<unit>
    abstract RemoveOpenCollections : JabberId option -> Task<unit>



type MessageArchivingClientPlugin 
    (stanzas : IXmlStanzaService, runtimeConfig : IRuntimeConfig, mgr : IXmppPluginManager, disco : IDiscoService) =
    // TODO: add service to check if it is supported!

    do
        // check if we need a store
        //if (not config.IsClientStream) then
        //    Configuration.configFail "MessageArchivingPlugin makes no sense on s2s streams"
        match runtimeConfig.StreamType with
        | ClientStream ->
            if (not runtimeConfig.IsInitializing) then
                if not <| mgr.HasPluginOf<MessagePlugin>() && not <| mgr.HasPluginOf<Server.IMServerPlugin>() then
                    Configuration.configFail "MessageArchivingClientPlugin requires an MessagePlugin or IMServerPlugin!"
        | _ -> ()


    let requestPreferences () = 
        async {
            let stanza = Parsing.createPreferenceElement (stanzas.GenerateNextId()) None PreferenceAction.RequestPreferences
            let! res = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            let result = Parsing.parseContentPreference res
            match result with
            | PreferenceAction.PreferenceResult prefResult ->
                return prefResult
            | _ -> return failwith "expected PreferenceResult.PreferenceResult message"
        } |> Log.TraceMe

    let sendRequest request = 
        let stanza = Parsing.createPreferenceElement (stanzas.GenerateNextId()) None request
        stanzas.QueueStanzaGeneric None stanza
    
    let saveCollection (col:ChatCollection) = 
        async {
            let stanza = Parsing.createManualArchivingElement (stanzas.GenerateNextId()) None (ManualArchivingAction.Save col)
            let! res = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            let result = Parsing.parseContentManualArchiving res
            match result with
            | ManualArchivingAction.SaveResult prefResult ->
                return prefResult
            | _ -> return failwith "expected ManualArchivingAction.SaveResult message"
        } |> Log.TraceMe  

    let setAutomaticSave (save:bool, scope) = 
        async {
            let stanza = Parsing.createAutomaticArchivingElement (stanzas.GenerateNextId()) (AutomaticArchivingAction.SetAutomaticArchiving (save, scope))
            let! res = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            return ()
        } |> Log.TraceMe
    
    let requestModifiedSince (date:System.DateTime) = 
        async {
            let stanza = Parsing.createReplicationElement (stanzas.GenerateNextId()) None (ReplicationAction.RequestModifiedSince date)
            let! res = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            let result = Parsing.parseContentReplication res
            match result with
            | ReplicationAction.RequestModifiedResult result ->
                return result
            | _ -> return failwith "expected ReplicationAction.RequestModifiedResult message"
        } |> Log.TraceMe
    
    let requestCollectionList (filter:CollectionFilter) = 
        async {
            let stanza = Parsing.createArchivingManagementElement (stanzas.GenerateNextId()) None (MessageArchivingAction.RequestList filter)
            let! res = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            let result = Parsing.parseContentArchivingManagement res
            match result with
            | MessageArchivingAction.RequestResult result ->
                return result
            | _ -> return failwith "expected MessageArchivingAction.RequestResult message"
        } |> Log.TraceMe
    
    let requestCollection (id:ChatCollectionId) = 
        async {
            let stanza = Parsing.createArchivingManagementElement (stanzas.GenerateNextId()) None (MessageArchivingAction.RequestCollection id)
            let! res = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            let result = Parsing.parseContentArchivingManagement res
            match result with
            | MessageArchivingAction.CollectionResult result ->
                return result
            | _ -> return failwith "expected MessageArchivingAction.CollectionResult message"
        } |> Log.TraceMe
    
    let removeCollection (id:ChatCollectionId) = 
        async {
            let stanza = Parsing.createArchivingManagementElement (stanzas.GenerateNextId()) None (MessageArchivingAction.RemoveCollection id)
            let! res = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            return ()
        } |> Log.TraceMe
    
    let removeAllCollections (filter:CollectionFilter) = 
        async {
            let stanza = Parsing.createArchivingManagementElement (stanzas.GenerateNextId()) None (MessageArchivingAction.RemoveAllCollections filter)
            let! res = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            return ()
        } |> Log.TraceMe
    
    let removeOpenCollections (filter:JabberId Option) = 
        async {
            let myOnResponse (rawStanza:Async<Stanza>) = 
                async {
                    let! res = rawStanza
                    return ()
                } |> Log.TraceMe

            let stanza = Parsing.createArchivingManagementElement (stanzas.GenerateNextId()) None (MessageArchivingAction.RemoveOpenCollections filter)
            let! ret = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            return ()
        } |> Log.TraceMe

    interface IMessageArchivingClientPluginService with
        member x.PrefRequest () = requestPreferences () |> Async.StartAsTaskImmediate
        member x.PrefRemoveItem(arg1: JabberId): unit = sendRequest (PreferenceAction.RemoveItem arg1)
        member x.PrefRemoveSession(arg1: ThreadId): unit = sendRequest (PreferenceAction.RemoveSession arg1)
        member x.PrefSetDefault(arg1: OtrSaveMode): unit = sendRequest (PreferenceAction.SetDefault arg1)
        member x.PrefSetItem(arg1: OtrItem): unit = sendRequest (PreferenceAction.SetItem arg1)
        member x.PrefSetMethods(arg1: AllMethodSettings): unit = sendRequest (PreferenceAction.SetMethods arg1)
        member x.PrefSetSession(arg1: SessionItem): unit = sendRequest (PreferenceAction.SetSession arg1)
        member x.RemoveAllCollections(arg1: CollectionFilter): Task<unit> = removeAllCollections arg1 |> Async.StartAsTaskImmediate
        member x.RemoveCollection(arg1: ChatCollectionId): Task<unit> = removeCollection arg1 |> Async.StartAsTaskImmediate
        member x.RemoveOpenCollections(arg1: JabberId option): Task<unit> = removeOpenCollections arg1 |> Async.StartAsTaskImmediate
        member x.RequestCollection(arg1: ChatCollectionId): Task<ChatCollection> = requestCollection arg1 |> Async.StartAsTaskImmediate
        member x.RequestCollectionList(arg1: CollectionFilter): Task<ChatCollectionHeader list> = requestCollectionList arg1 |> Async.StartAsTaskImmediate
        member x.RequestModifiedSince(arg1: System.DateTime): Task<ChangeItem list> = requestModifiedSince arg1 |> Async.StartAsTaskImmediate
        member x.SaveCollection(arg1: ChatCollection): Task<ChatCollectionHeader> = saveCollection arg1 |> Async.StartAsTaskImmediate
        member x.SetAutomaticSave(arg1: bool, scope): Task<unit> = setAutomaticSave (arg1, scope) |> Async.StartAsTaskImmediate

    interface IXmppPlugin with
        member x.Name = "MessageArchivingClientPlugin"
        member x.PluginService = Service.FromInstance<IMessageArchivingClientPluginService, _> x 
