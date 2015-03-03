// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiving

open Yaaf.Xmpp
open Yaaf.Logging

type StoredPreferenceInfo =
  { AutomaticArchiving : bool option
    DefaultOtrSaveMode : OtrSaveMode
    ItemSetting : OtrItem list
    MethodSetting : AllMethodSettings option } with
    static member Default =
      { AutomaticArchiving = None
        DefaultOtrSaveMode = OtrSaveMode.Empty
        ItemSetting = []
        MethodSetting = None }
        

type IUserPreferenceStore = 
    
    // Preferences 
    /// Get the user preferences of a specific user, returns None when the user never saved anything, the bool indicates whether 'archiving method preferences' where set.
    abstract GetUserPreferences : unit -> (StoredPreferenceInfo) option Task
    
    /// Save the user preferences of a specific user, returns None when the user never saved anything
    // abstract SetUserPreferences : StoredPreferenceInfo -> unit

    /// Set the default preferences for all items (expire, otrmode, savemode). None deletes this preferences and uses the default next time!
    abstract SetDefaultOtrSaveMode : OtrSaveMode option ->  System.Threading.Tasks.Task
    /// Save preferences for an item.
    abstract SetItem : JabberId * OtrSaveMode ->  System.Threading.Tasks.Task
    /// Set method preferences, None deletes this key and restores the defaults
    abstract SetMethodPreferences : AllMethodSettings option ->  System.Threading.Tasks.Task
    /// Remove a previously set item.
    abstract RemoveItem : JabberId ->  System.Threading.Tasks.Task
    /// Save preference for archiving, None removes this setting and restores server defauls.
    abstract SetArchiving : bool option ->  System.Threading.Tasks.Task


type IUserArchivingStore = 
    
    // Manual / Auto
    /// Save the given Collection for the given user in the database, should take care to update if the item exists
    /// Messages: should be appended if there exists a collection
    /// Subject: should be changed when available but only deleted when no messages are given
    abstract StoreCollection : ChatCollection -> System.Threading.Tasks.Task
    
    // Management
    /// Filters the Collections of the given user by the given filter and returns the collection headers
    abstract FilterMessages : CollectionFilter -> ChatCollectionHeader list Task
    
    /// Retrieves the specified Collection
    abstract RetrieveCollection : ChatCollectionId -> ChatCollection Task
    
    /// Removes the specified Collection, note that the concrete implementation has to save the info that this collection was removed
    /// because it should notify about the removal in GetChangesSince
    abstract RemoveCollection : ChatCollectionId -> bool Task
    
    // Replication
    /// Returns all changes since the given date
    abstract GetChangesSince : System.DateTime -> ChangeItem list Task


type IMessageArchivingStore = 
    //inherit IUserPreferenceStore
    abstract member GetPreferenceStore : JabberId -> IUserPreferenceStore Task
    //inherit IUserArchivingStore
    abstract member GetArchiveStore : JabberId -> IUserArchivingStore Task

type MemoryUserPreferenceStore (jid:JabberId) = 
    let mutable preference = None
    static let completed = Task.FromResult () :> System.Threading.Tasks.Task
    let getPrefInfo () =
        match preference with
        | Some s -> s
        | None -> StoredPreferenceInfo.Default

    interface IUserPreferenceStore with
        member x.GetUserPreferences () = 
            match preference with
            | Some s -> 
                if s.AutomaticArchiving.IsNone && s.DefaultOtrSaveMode = OtrSaveMode.Empty && s.ItemSetting.IsEmpty && s.MethodSetting.IsNone then
                    Task.FromResult None
                else Task.FromResult <| Some s
            | None -> Task.FromResult <| None
        //member x.SetUserPreferences prefInfo = preference <- Some prefInfo
        member x.SetDefaultOtrSaveMode mode = 
            preference <- Some { getPrefInfo() with DefaultOtrSaveMode = match mode with Some m -> m | None -> OtrSaveMode.Empty }
            completed
        member x.SetItem (jid, pref) = 
            let curPref = getPrefInfo()
            let hasFound = ref false
            let newList =
                curPref.ItemSetting |> 
                    List.map (fun (itemJid, itemPref) -> 
                        if (itemJid.FullId = jid.FullId) then 
                            hasFound := true
                            (jid, pref) 
                        else 
                            (itemJid, itemPref))
            preference <- Some { curPref with ItemSetting = if !hasFound then newList else (jid,pref) :: newList }
            completed
        member x.SetMethodPreferences methodsPrefs = 
            let curPref = getPrefInfo()
            preference <- Some { curPref with MethodSetting = methodsPrefs }
            completed
        member x.RemoveItem jid =
            let curPref = getPrefInfo()
            preference <- Some { curPref with ItemSetting = curPref.ItemSetting |> List.filter (fun (itemJid,_) -> itemJid.FullId <> jid.FullId) }
            completed
        member x.SetArchiving archiving =
            preference <- Some { getPrefInfo() with AutomaticArchiving = archiving }
            completed
        
type MemoryUserArchivingStore (jid:JabberId) = 
    let msgStore = new System.Collections.Concurrent.ConcurrentDictionary<_, _>()
    let changes = new System.Collections.Concurrent.ConcurrentDictionary<ChatCollectionId,System.Collections.Concurrent.ConcurrentDictionary<System.Int64, ChangeItem>>()
    let getChangeList (id) = 
        changes.GetOrAdd(
            id,
            new System.Func<_, _>(fun id -> 
                new System.Collections.Concurrent.ConcurrentDictionary<_,_>()))

    static let completed = Task.FromResult () :> System.Threading.Tasks.Task
    let stripDate date = (StreamData.DateTime.ToString date |> StreamData.DateTime.Parse).Ticks
    //let currentChangeList () = 
    //    let now = System.DateTime.Now
    //    let stripped = stripDate now
    //    getChangeList stripped

    interface IUserArchivingStore with
        member x.StoreCollection col =
            let version =
                match msgStore.TryGetValue col.Header.Id with
                | true, oldCol -> oldCol.Header.Version.Value + 1
                | _ -> 0
            let col =
                { col with Header = { col.Header with Version = Some version }}
            msgStore.[col.Header.Id] <- col
            let change = ChangeItemType.Changed
            let date = stripDate System.DateTime.Now
            Log.Verb (fun _ -> L "StoreCollection: %A (Date: %A)" col date)
            let changeList = getChangeList col.Header.Id
            let changeItem =  { Type = change; Data = { Version = col.Header.Version.Value; Id = col.Header.Id} }
            changeList.AddOrUpdate(date, changeItem, new System.Func<_,_,_>(fun a b -> changeItem))
            |> ignore
            completed

        member x.FilterMessages filter =
            Log.Verb (fun _ -> L "FilterMessages: %A" filter)
            msgStore.Values
                |> Seq.map (fun col -> col.Header)
                |> Seq.filter (fun col -> filter.Filter col)
                |> Seq.toList
                |> Task.FromResult
        member x.RetrieveCollection  id =
            Log.Verb (fun _ -> L "RetrieveCollection: %A" id)
            msgStore.[id] |> Task.FromResult

        member x.RemoveCollection id =
            match msgStore.TryRemove id with
            | true, col ->
                let change = ChangeItemType.Removed
                let changeList = getChangeList id
                let date = stripDate System.DateTime.Now
                Log.Verb (fun _ -> L "RemoveCollection: %A (date: %A)" id date)
                let changeItem = { Type = change; Data = { Version = col.Header.Version.Value; Id = id } }
                changeList.AddOrUpdate(date, changeItem, new System.Func<_,_,_>(fun _ _ -> changeItem)) |> ignore
                true
            | _ -> false 
            |> Task.FromResult

        member x.GetChangesSince date =
            let stripped = stripDate date
            Log.Verb (fun _ -> L "GetChangesSince: %A" stripped)
            changes 
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> Seq.map 
                (fun (id, data) -> 
                    data
                    |> Seq.map (fun kv -> kv.Key, kv.Value)
                    |> Seq.filter (fun (k, v) -> 
                        let res = k >= stripped
                        Log.Verb (fun _ -> L "k (%A) >= stripped (%A): %A" k stripped res)
                        res)
                    |> Seq.maxBy (fun (id, date) -> date)
                    |> snd
                    |> (fun item -> id, item))
            //|> Seq.collect id
            //|> Seq.collect id
            |> Seq.map snd
            |> Seq.toList
            |> Task.FromResult

type MemoryMessageArchivingStore () = 
    let userPrefStore = new System.Collections.Generic.Dictionary<_,_>()
    let userColStore = new System.Collections.Generic.Dictionary<_, _>()
    interface IMessageArchivingStore with
        member x.GetPreferenceStore jid =
            Log.Verb (fun _ -> L "GetPreferenceStore: %s" jid.FullId)
            match userPrefStore.TryGetValue jid.BareId with
            | true, v -> Task.FromResult v
            | false, _ -> 
                let newStore = MemoryUserPreferenceStore(jid.BareJid) :> IUserPreferenceStore
                userPrefStore.Add(jid.BareId, newStore)
                Task.FromResult newStore
        member x.GetArchiveStore jid  =
            Log.Verb (fun _ -> L "GetArchiveStore: %s" jid.FullId)
            match userColStore.TryGetValue jid.BareId with
            | true, v -> Task.FromResult v
            | false, _ -> 
                let newStore = MemoryUserArchivingStore(jid.BareJid) :> IUserArchivingStore
                userColStore.Add(jid.BareId, newStore)
                Task.FromResult  newStore