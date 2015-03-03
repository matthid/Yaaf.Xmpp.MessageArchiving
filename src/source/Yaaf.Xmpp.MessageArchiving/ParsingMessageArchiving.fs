// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiving

open Yaaf.Xmpp
open System.Xml.Linq
open Yaaf.Xmpp.XmlStanzas

open System.Xml.Linq
open Yaaf.Xml
open Yaaf.Helper
open Yaaf.Xmpp.XmlStanzas.Parsing
// Preferences
type SaveMode =
    /// the saving entity SHOULD save only <body/> elements.
    /// Note: When archiving locally a client MAY save the full XML content of each <message/> element even if the Save Mode is 'body'.
    | Body
    /// the saving entity MUST save nothing (false).
    | Nothing
    /// the saving entity SHOULD save the full XML content of each <message/> element.
    /// Note: Support for the 'message' value is optional and, to conserve bandwidth and storage space, it is RECOMMENDED that client implementations do not specify the 'message' value.
    | Message
    /// the saving entity SHOULD save every byte that passes over the stream in either direction.
    | Stream with 
    static member Parse s = 
        match s with 
        | "body" -> Body
        | "false" -> Nothing
        | "message" ->  Message
        | "stream" ->  Stream
        | _ -> failwith "unknown data in otr attribute"
    member x.AttributeValue = 
        match x with
        | Body -> "body" 
        | Nothing -> "false" 
        | Message  -> "message" 
        | Stream  -> "stream" 
type OtrMode = 
    /// the user MUST explicitly approve off-the-record communication.
    | Approve
    /// communications MAY be off the record if requested by another user.
    | Concede
    /// communications MUST NOT be off the record.
    | Forbid
    /// communications SHOULD NOT be off the record even if requested.
    | Oppose
    /// communications SHOULD be off the record if possible.
    | Prefer
    /// communications MUST be off the record. *
    | Require with
    static member Parse s = 
        match s with 
        | "approve" -> Approve
        | "concede" -> Concede
        | "forbid" ->  Forbid
        | "oppose" ->  Oppose
        | "prefer" ->  Prefer
        | "require" -> Require
        | _ -> failwith "unknown data in otr attribute"
    member x.AttributeValue = 
        match x with
        | Approve -> "approve" 
        | Concede -> "concede" 
        | Forbid  -> "forbid" 
        | Oppose  -> "oppose" 
        | Prefer  -> "prefer" 
        | Require -> "require" 
type OtrSaveMode = 
  { Expire : System.Int64 option
    OtrMode : OtrMode option
    SaveMode : SaveMode option } with
    static member FromElemAttributes (x:XElement) = 
      { Expire = x |> tryXAttrValue (getXName "expire" "") |> Option.map StreamData.Long.Parse
        OtrMode = x |> tryXAttrValue (getXName "otr" "") |> Option.map OtrMode.Parse
        SaveMode = x |> tryXAttrValue (getXName "save" "") |> Option.map SaveMode.Parse }
    static member Empty  = 
      { Expire = None
        OtrMode = None
        SaveMode = None }
    member x.CreateAttributes () = 
      [ 
        if x.Expire.IsSome then
            yield XAttribute(getXName "expire" "", StreamData.Long.ToString x.Expire.Value)
        if x.OtrMode.IsSome then
            yield XAttribute(getXName "otr" "", x.OtrMode.Value.AttributeValue)
        if x.SaveMode.IsSome then
            yield XAttribute(getXName "save" "", x.SaveMode.Value.AttributeValue)
      ]
type OtrItem = JabberId * OtrSaveMode
type ThreadId = string
type SessionItem = ThreadId * OtrSaveMode
type UseType = 
    /// this method MAY be used if no other methods are available.
    | Concede
    /// this method MUST NOT be used.
    | Forbid
    /// this method SHOULD be used if available.
    | Prefer with 
    static member Parse s = 
        match s with 
        | "concede" -> Concede
        | "forbid" -> Forbid
        | "prefer" -> Prefer
        | _ -> failwith "unknown data in use attribute"
    member x.AttributeValue = 
        match x with
        | Concede -> "concede"
        | Forbid -> "forbid"
        | Prefer -> "prefer"
type MethodType = 
    /// preferences for use of automatic archiving on the user's server.
    | Auto
    /// references for use of local archiving to a file or database on the user's machine or device.
    | Local
    /// preferences for use of manual archiving by the user's client to the user's server.
    | Manual with 
    static member Parse s = 
        match s with 
        | "auto" -> Auto
        | "local" -> Local
        | "manual" -> Manual
        | _ -> failwith "unknown type in method attribute"
    member x.AttributeValue = 
        match x with
        | Auto -> "auto"
        | Local -> "local"
        | Manual -> "manual"

type MethodSetting = 
  { Type : MethodType
    Use : UseType}
type AllMethodSettings = 
  { Auto : UseType
    Local : UseType
    Manual : UseType } with
    static member FromDict (dict:System.Collections.Generic.IDictionary<_,_>) = 
      { Auto = dict.[MethodType.Auto]
        Local = dict.[MethodType.Local]
        Manual = dict.[MethodType.Manual] }
    
type ArchivingScope = 
    /// the setting will remain for next streams.
    | Global
    /// (default) the setting is true only until the end of the stream. For next stream, server default value will be used.
    | Stream with 
    static member Parse s = 
        match s with 
        | "global" -> Global
        | "stream" -> Stream
        | _ -> failwith "unknown data in scope attribute"
    member x.AttributeValue = 
        match x with
        | Global -> "global"
        | Stream -> "stream"
type PreferenceInfo =
  { AutomaticArchiving : bool
    AutomaticArchivingScope : ArchivingScope option
    DefaultModesUnset : bool option
    DefaultOtrSaveMode : OtrSaveMode
    ItemSetting : OtrItem list
    SessionSetting : SessionItem list
    MethodSetting : AllMethodSettings }
type PreferenceAction = 
    | RequestPreferences
    | PreferenceResult of PreferenceInfo
    | SetDefault of OtrSaveMode
    | SetItem of OtrItem
    | RemoveItem of JabberId
    | SetSession of SessionItem
    | RemoveSession of ThreadId
    | SetMethods of AllMethodSettings

type PrefStanza = Stanza<PreferenceAction>


// Manual Archiving
open System
type ChatContent = 
   { Body : string option
     AdditionalData : XElement list }
type MessageInfo =
  { Sec : int option
    Utc : DateTime option
    Name : string option
    Jid : JabberId option } with
    static member FromAttributes (e:XElement) = 
     { Sec = e |> tryXAttrValue (getXName "secs" "") |> Option.map System.Int32.Parse
       Utc = e |> tryXAttrValue (getXName "utc" "") |> Option.map StreamData.DateTime.Parse
       Name = e |> tryXAttrValue (getXName "name" "")
       Jid = e |> tryXAttrValue (getXName "jid" "") |> Option.map JabberId.Parse }
    member x.CreateAttributes () = 
      [ 
        if x.Sec.IsSome then
            yield XAttribute(getXName "secs" "", x.Sec.Value.ToString())
        if x.Utc.IsSome then
            yield XAttribute(getXName "utc" "", x.Utc.Value.ToUniversalTime().ToString("s") + "Z")
        if x.Name.IsSome then
            yield XAttribute(getXName "name" "", x.Name.Value)
        if x.Jid.IsSome then
            yield XAttribute(getXName "jid" "", x.Jid.Value.BareId)
      ]
type ChatCollectionId = 
  { With : JabberId
    Start : DateTime } with
    static member FromAttributes (e:XElement) = 
     { Start = e |> forceAttrValue (getXName "start" "") |> StreamData.DateTime.Parse
       With = e |> forceAttrValue (getXName "with" "") |> JabberId.Parse }
    member x.CreateAttributes () = 
      [ 
        yield XAttribute(getXName "start" "", StreamData.DateTime.ToString x.Start)
        yield XAttribute(getXName "with" "", x.With.FullId)
      ]
type ChatItem = 
    | From of MessageInfo * ChatContent
    | To of MessageInfo * ChatContent
    | Note of string * DateTime Option with
    member x.IsToItem 
        with get () =
            match x with
            | To _ -> true
            | _ -> false
    member x.IsFromItem 
        with get () =
            match x with
            | From _ -> true
            | _ -> false
    member x.IsNoteItem 
        with get () =
            match x with
            | Note _ -> true
            | _ -> false
    member x.Date 
        with get() =
            match x with
            | To (info, _) 
            | From (info, _) -> info.Utc
            | Note (_, date) -> date
    member x.Message 
        with get() =
            match x with
            | To (_, content) 
            | From (_, content) -> content.Body.Value
            | Note (content, _) -> content
    

type ChatCollectionHeader =
  { Id : ChatCollectionId
    Thread : string option
    Version : int option
    Subject : string option }
    static member FromAttributes (e:XElement) = 
     { Id = ChatCollectionId.FromAttributes e
       Thread =  e |> tryXAttrValue (getXName "thread" "") 
       Subject =  e |> tryXAttrValue (getXName "subject" "")
       Version =  e |> tryXAttrValue (getXName "version" "") |> Option.map System.Int32.Parse }
    member x.CreateAttributes () = 
      [ 
        yield! x.Id.CreateAttributes()
        if x.Thread.IsSome then
            yield XAttribute(getXName "thread" "", x.Thread.Value)
        if x.Version.IsSome then
            yield XAttribute(getXName "version" "", x.Version.Value.ToString())
        if x.Subject.IsSome then
            yield XAttribute(getXName "subject" "", x.Subject.Value)
      ]
type ChatCollection = 
  { Header : ChatCollectionHeader
    ChatItems : ChatItem list
    // flag used to indicate if the "Previous" value should be used for overwriting an existing "Previous" value
    SetPrevious : bool
    Previous : ChatCollectionId option
    // See SetPrevious flag for an explanation
    SetNext : bool
    Next : ChatCollectionId option } with
    static member FromHeader header = 
      { Header = header
        ChatItems = []
        SetPrevious = false
        Previous = None
        SetNext = false
        Next = None }

type ManualArchivingAction = 
    | Save of ChatCollection
    | SaveResult of ChatCollectionHeader
    //| SetSubject of ChatCollectionId * string
    //| SetPreviousNext of ChatCollectionId * (ChatCollectionId option) * (ChatCollectionId option)
    //| SetPrevious of ChatCollectionId * (ChatCollectionId option)
    //| SetNext of ChatCollectionId * (ChatCollectionId option)
    
type ManualArchivingStanza = Stanza<ManualArchivingAction>


(*6. Automatic Archiving
<message from='capulet.com' to='juliet@capulet.com/chamber'>
  <body>WARNING: All messages that you send or
        receive will be recorded by the server.</body>
</message>*)
type AutomaticArchivingAction = 
    | SetAutomaticArchiving of bool * (ArchivingScope option)
type AutomaticArchivingStanza = Stanza<AutomaticArchivingAction>

// 7. Archive Management
type CollectionFilter = 
  { With : JabberId option
    Start : DateTime option
    End : DateTime option } with
    static member FromAttributes (e:XElement) = 
     { Start = e |> tryXAttrValue (getXName "start" "") |> Option.map StreamData.DateTime.Parse
       With = e |> tryXAttrValue (getXName "with" "") |> Option.map JabberId.Parse
       End = e |> tryXAttrValue (getXName "end" "") |> Option.map StreamData.DateTime.Parse }
    static member Any  = 
     { Start = None
       With = None
       End = None }
    member x.After date = { x with Start = Some date }
    member x.Before date = { x with End = Some date }
    member x.WithPartner jid = { x with With = Some jid }
    member x.CreateAttributes () = 
      [ 
        if x.With.IsSome then
            yield XAttribute(getXName "with" "", x.With.Value.FullId)
        if x.Start.IsSome then
            yield XAttribute(getXName "start" "", x.Start.Value |> StreamData.DateTime.ToString)
        if x.End.IsSome then
            yield XAttribute(getXName "end" "", x.End.Value |> StreamData.DateTime.ToString)
      ]
    member x.Filter (col:ChatCollectionHeader) =
        let withOk = 
            match x.With with
            | Some w -> 
                col.Id.With.IsSpecialOf w
            | None -> true
        let startOk =
            match x.Start with
            | Some d ->
                col.Id.Start >= d
            | None -> true
        let endOk = 
            match x.End with
            | Some d ->
                col.Id.Start <= d
            | None -> true
        withOk && startOk && endOk
type MessageArchivingAction =
    /// 7.1 Retrieving a List of Collections
    | RequestList of CollectionFilter
    /// Result of RequestList
    | RequestResult of ChatCollectionHeader list
    /// 7.2 Retrieving a Collection
    | RequestCollection of ChatCollectionId
    /// Result of RequestCollection
    | CollectionResult of ChatCollection
    /// 7.3 Removing a Collection
    | RemoveCollection of ChatCollectionId
    /// To remove ALL collections set all fields in RequestCollectionFilter to None
    /// This removes all collections which would be returned with RequestList.
    | RemoveAllCollections of CollectionFilter
    /// 7.3 Removing a Collection
    | RemoveOpenCollections of JabberId option
    
type RetrieveActionStanza = Stanza<MessageArchivingAction>

// Replication
type ChangeItemType =
    | Changed
    | Removed
type ChangeItemData =
  { Version : int
    Id : ChatCollectionId } with
    static member FromAttributes (e:XElement) = 
     { Id = ChatCollectionId.FromAttributes e
       Version = e |> forceAttrValue (getXName "version" "") |> StreamData.Int.Parse }
    member x.CreateAttributes () = 
      [ 
        yield! x.Id.CreateAttributes()
        yield XAttribute(getXName "version" "", x.Version |> StreamData.Int.ToString)
      ]
type ChangeItem = 
  { Data : ChangeItemData
    Type : ChangeItemType}
type ReplicationAction = 
    | RequestModifiedSince of DateTime
    | RequestModifiedResult of ChangeItem list

type ReplicationActionStanza = Stanza<ReplicationAction>


module Parsing = 

    let messageArchiveNs = "urn:xmpp:archive"

    // Shared by all

    let parseChatItemContent (e:XElement) = 
        let bodyElem = 
            e.Elements()
            |> Seq.tryFind (fun e -> e.Name = getXName "body" messageArchiveNs)
        { Body = bodyElem |> Option.map (fun e -> e.Value)
          ChatContent.AdditionalData = 
            e.Elements()
            |> Seq.filter (fun e -> e.Name <> getXName "body" messageArchiveNs)
            |> Seq.append 
                (e.Elements()
                    |> Seq.filter (fun e -> e.Name = getXName "body" messageArchiveNs)
                    |> Seq.skip 1)
            |> Seq.toList }
    
    let parseChatItem (e:XElement) = 
        match e.Name.LocalName, e.Name.NamespaceName with
        | "to", Equals messageArchiveNs ->
            ChatItem.To(MessageInfo.FromAttributes e, parseChatItemContent e)
        | "from", Equals messageArchiveNs ->
            ChatItem.From(MessageInfo.FromAttributes e, parseChatItemContent e)
        | "note", Equals messageArchiveNs ->
            let utc = e |> tryXAttrValue (getXName "utc" "") |> Option.map StreamData.DateTime.Parse
            ChatItem.Note(e.Value, utc)
        | _ -> failwith "unknown chat item"

    let parseChatElem (chatElem:XElement) = 
        //let chatElem = 
        //    saveItem.Elements()
        //    |> Seq.find (fun e -> e.Name = getXName "chat" messageArchiveNs)
        let prevElem = 
            chatElem.Elements()
            |> Seq.tryFind (fun e -> e.Name = getXName "previous" messageArchiveNs)
        let prevElemAvailableAndNotEmpty = prevElem.IsSome && (prevElem.Value.Attributes() |> Seq.isEmpty |> not)
        let nextElem = 
            chatElem.Elements()
            |> Seq.tryFind (fun e -> e.Name = getXName "next" messageArchiveNs)
        let nextElemAvailableAndNotEmpty = nextElem.IsSome && (nextElem.Value.Attributes() |> Seq.isEmpty |> not)
        
        let validChatItems = [ "to"; "from"; "note" ]
        let chatElems =
            chatElem.Elements()
            |> Seq.filter (fun e -> validChatItems |> Seq.exists (fun name -> e.Name = getXName name messageArchiveNs))
            |> Seq.map parseChatItem
        { Header = ChatCollectionHeader.FromAttributes chatElem
          ChatItems = chatElems |> Seq.toList
          SetPrevious = prevElem.IsSome 
          Previous = 
              if prevElemAvailableAndNotEmpty then
                  prevElem |> Option.map (ChatCollectionId.FromAttributes)
              else None
          SetNext = nextElem.IsSome
          Next = 
              if nextElemAvailableAndNotEmpty then
                  nextElem |> Option.map (ChatCollectionId.FromAttributes)
              else None }

    let createChatItemElement (chatItem:ChatItem) = 
        let createContent (content:ChatContent) = 
            [
                if content.Body.IsSome then
                    yield [ content.Body.Value ] |> getXElemWithChilds (getXName "body" messageArchiveNs) 
                yield! content.AdditionalData
            ]
        match chatItem with
        | ChatItem.From (info, content) ->
            info.CreateAttributes() 
            |> Seq.cast<obj>
            |> Seq.append (createContent content |> Seq.cast<obj>) 
            |> getXElemWithChilds (getXName "from" messageArchiveNs) 
        | ChatItem.To (info, content) ->
            info.CreateAttributes() 
            |> Seq.cast<obj>
            |> Seq.append (createContent content |> Seq.cast<obj>) 
            |> getXElemWithChilds (getXName "to" messageArchiveNs) 
        | ChatItem.Note (note, utc) ->
            [ 
                if utc.IsSome then
                    yield XAttribute(getXName "utc" "", StreamData.DateTime.ToString utc.Value) :> obj
                yield note :> obj
            ] |> getXElemWithChilds (getXName "note" messageArchiveNs) 
                    
    let createChatElement (col:ChatCollection) = 
        [
            yield! col.Header.CreateAttributes() |> Seq.cast
            if col.SetPrevious then
                let childs =
                    match col.Previous with
                    | Some prevElem ->
                        prevElem.CreateAttributes()
                    | None ->
                        []
                yield childs |> getXElemWithChilds (getXName  "previous" messageArchiveNs) :> obj
            if col.SetNext then
                let childs =
                    match col.Next with
                    | Some nextElem ->
                        nextElem.CreateAttributes()
                    | None ->
                        []
                yield childs |> getXElemWithChilds (getXName  "next" messageArchiveNs) :> obj
            yield!
                col.ChatItems |> Seq.map createChatItemElement |> Seq.cast
        ] 
        |> getXElemWithChilds (getXName "chat" messageArchiveNs)

    // Preference Parsing

    let isContentPreference (stanza:IStanza) = 
        if stanza.Header.StanzaType <> XmlStanzaType.Iq  then false
        else
        let queryItem =
            stanza.Contents.Children 
            |> Seq.tryFind 
                (fun e -> 
                    [ "pref"; "sessionremove"; "itemremove" ]
                    |> Seq.exists (fun name -> e.Name = getXName name messageArchiveNs))
        queryItem.IsSome 

    let parseContentPreference (stanza:IStanza) =
        if stanza.Header.StanzaType <> XmlStanzaType.Iq then failwith "expected iq stanza"
        else
        let findItems (e:XElement) = 
            e.Elements()
            |> Seq.filter (fun e -> e.Name = getXName "item" messageArchiveNs)
            |> Seq.map 
                (fun e -> 
                    (forceAttrValue (getXName "jid" "") e |> JabberId.Parse, 
                        OtrSaveMode.FromElemAttributes e):OtrItem)
        let findSession (e:XElement) = 
            e.Elements()
            |> Seq.filter (fun e -> e.Name = getXName "session" messageArchiveNs)
            |> Seq.map (fun e -> (forceAttrValue (getXName "thread" "") e, OtrSaveMode.FromElemAttributes e):SessionItem)
        let findMethod (e:XElement) = 
            e.Elements()
            |> Seq.filter (fun e -> e.Name = getXName "method" messageArchiveNs)
            |> Seq.map 
                (fun e -> 
                    forceAttrValue (getXName "type" "") e |> MethodType.Parse, 
                    forceAttrValue (getXName "use" "") e |> UseType.Parse)
            |> dict
            |> AllMethodSettings.FromDict
        let findDefault (e:XElement) = 
            e.Elements()
            |> Seq.tryFind (fun e -> e.Name = getXName "default" messageArchiveNs)
            |> Option.map 
                (fun defaultItem ->
                    OtrSaveMode.FromElemAttributes defaultItem,
                    defaultItem |> tryXAttrValue (getXName "unset" "") |> Option.map bool.Parse)
        let prefItem =
            stanza.Contents.Children 
            |> Seq.tryFind (fun e -> e.Name = getXName "pref" messageArchiveNs)
        match prefItem with
        | Some prefItem ->
            match stanza.Header.Type.Value with
            | "get" -> 
                assert (prefItem.Elements() |> Seq.isEmpty)
                PreferenceAction.RequestPreferences
            | "result" ->
                let autoItem =
                    prefItem.Elements()
                    |> Seq.find (fun e -> e.Name = getXName "auto" messageArchiveNs)
                let saveOption = autoItem |> forceAttrValue (getXName "save" "") |> bool.Parse
                let saveOptionScope = autoItem |> tryXAttrValue (getXName "scope" "") |> Option.map ArchivingScope.Parse
                let defaultSaveMode, defaultSaveModeUnset =
                    findDefault prefItem |> Option.get
                let itemOptions = findItems prefItem
                let sessionOptions = findSession prefItem
                let methodOptions = findMethod prefItem
                let preferences = 
                  { AutomaticArchiving = saveOption
                    AutomaticArchivingScope = saveOptionScope
                    DefaultModesUnset = defaultSaveModeUnset
                    DefaultOtrSaveMode = defaultSaveMode
                    ItemSetting = itemOptions |> Seq.toList
                    SessionSetting = sessionOptions |> Seq.toList
                    MethodSetting = methodOptions } : PreferenceInfo
                PreferenceAction.PreferenceResult preferences
            | "set" ->
                let def = findDefault prefItem
                let itemOptions = findItems prefItem
                let sessionOptions = findSession prefItem
                if def.IsSome then
                    let s, _ = def.Value
                    PreferenceAction.SetDefault s
                elif itemOptions |> Seq.isEmpty |> not then
                    let item = itemOptions |> Seq.exactlyOne
                    PreferenceAction.SetItem item
                elif sessionOptions |> Seq.isEmpty |> not then
                    let session = sessionOptions |> Seq.exactlyOne
                    PreferenceAction.SetSession session
                else
                    let methods = findMethod prefItem
                    PreferenceAction.SetMethods methods
            | _ -> failwithf "unknown type in disco stanza: %s" stanza.Header.Type.Value
        | None ->
            let itemRemoveItem =
                stanza.Contents.Children 
                |> Seq.tryFind (fun e -> e.Name = getXName "itemremove" messageArchiveNs)
            let sessionRemoveItem =
                stanza.Contents.Children 
                |> Seq.tryFind (fun e -> e.Name = getXName "sessionremove" messageArchiveNs)
            if itemRemoveItem.IsSome then
                PreferenceAction.RemoveItem (findItems itemRemoveItem.Value |> Seq.exactlyOne |> fst)
            elif sessionRemoveItem.IsSome then
                PreferenceAction.RemoveSession (findSession sessionRemoveItem.Value |> Seq.exactlyOne |> fst)
            else
                failwith "unknown PreferenceAction element!"
                
    let parsePreferenceStanza ns (elem:XElement) =  // parse something within the "stream"
        parseGenericStanza ns (parseContentPreference) elem

    let createPreferenceStanzaElement (content:PreferenceAction) = 
        let createItemElem ((jid, item):OtrItem) = 
            [
                yield XAttribute(getXName "jid" "", jid.BareId)
                yield! item.CreateAttributes()
            ] |> getXElemWithChilds (getXName  "item" messageArchiveNs)
        let createSessionElem ((thread, item):SessionItem) = 
            [
                yield XAttribute(getXName "thread" "", thread)
                yield! item.CreateAttributes()
            ] |> getXElemWithChilds (getXName  "session" messageArchiveNs)
        let createMethods (methd:AllMethodSettings) = 
            [ (MethodType.Auto, methd.Auto); (MethodType.Local, methd.Local); MethodType.Manual,methd.Manual]
            |> Seq.map
                (fun (t,u) ->
                    [
                        yield XAttribute(getXName "type" "", t.AttributeValue)
                        yield XAttribute(getXName "use" "", u.AttributeValue)
                    ] |> getXElemWithChilds (getXName  "method" messageArchiveNs)
                 )
            
        match content with
        | PreferenceAction.RequestPreferences ->
            getXElem (getXName "pref" messageArchiveNs)
        | PreferenceAction.PreferenceResult res ->
            [
                yield 
                  [
                    yield XAttribute(getXName "save" "", res.AutomaticArchiving)
                    if res.AutomaticArchivingScope.IsSome then
                        yield XAttribute(getXName "scope" "", res.AutomaticArchivingScope.Value.AttributeValue)
                  ] |> getXElemWithChilds (getXName  "auto" messageArchiveNs)
                yield
                  [
                    yield! (res.DefaultOtrSaveMode.CreateAttributes())
                    if res.DefaultModesUnset.IsSome then
                        yield XAttribute(getXName "unset" "", res.DefaultModesUnset.Value)
                  ] |> getXElemWithChilds (getXName  "default" messageArchiveNs)
                yield! 
                    res.ItemSetting
                    |> Seq.map createItemElem
                yield! 
                    res.SessionSetting
                    |> Seq.map createSessionElem
                yield! 
                    res.MethodSetting |> createMethods
            ] |> getXElemWithChilds (getXName  "pref" messageArchiveNs)
        | PreferenceAction.SetDefault def ->
            [
                yield
                  [
                    yield! (def.CreateAttributes())
                  ] |> getXElemWithChilds (getXName  "default" messageArchiveNs)
            ] |> getXElemWithChilds (getXName  "pref" messageArchiveNs)
        | PreferenceAction.SetItem item ->
            [
                yield createItemElem item
            ] |> getXElemWithChilds (getXName  "pref" messageArchiveNs)
        | PreferenceAction.SetSession item ->
            [
                yield createSessionElem item
            ] |> getXElemWithChilds (getXName  "pref" messageArchiveNs)
        | PreferenceAction.SetMethods meths ->
            [
                yield! createMethods meths
            ] |> getXElemWithChilds (getXName  "pref" messageArchiveNs)
        | PreferenceAction.RemoveItem jid ->
            [
                yield createItemElem (jid, OtrSaveMode.Empty)
            ] |> getXElemWithChilds (getXName  "itemremove" messageArchiveNs)
        | PreferenceAction.RemoveSession thread ->
            [
                yield createSessionElem (thread, OtrSaveMode.Empty)
            ] |> getXElemWithChilds (getXName  "sessionremove" messageArchiveNs)

    let preferenceContentGenerator = ContentGenerator.SimpleGenerator createPreferenceStanzaElement
    
    let createPreferenceElement (id:string) (target:JabberId option) (data:PreferenceAction) = 
        let cType = 
            match data with
            | PreferenceAction.RequestPreferences _ -> "get"
            | PreferenceAction.PreferenceResult _ -> "result"
            | _ -> "set"
        Stanza<_>.CreateGen preferenceContentGenerator
          { To = target
            From = None
            Id = Some id
            Type = Some cType
            StanzaType = XmlStanzaType.Iq }
          data

    // Manual Archiving Parsing

    let isContentManualArchiving (stanza:IStanza) = 
        if stanza.Header.StanzaType <> XmlStanzaType.Iq  then false
        else
        let queryItem =
            stanza.Contents.Children 
            |> Seq.tryFind 
                (fun e -> e.Name = getXName "save" messageArchiveNs)
        queryItem.IsSome 

    let parseContentManualArchiving (stanza:IStanza) =
        if stanza.Header.StanzaType <> XmlStanzaType.Iq then failwith "expected iq stanza"
        else

        let saveItem =
            stanza.Contents.Children 
            |> Seq.find (fun e -> e.Name = getXName "save" messageArchiveNs)

        let parsedData = 
            saveItem.Elements()
            |> Seq.find (fun e -> e.Name = getXName "chat" messageArchiveNs)
            |> parseChatElem

        match stanza.Header.Type.Value with
        | "set" -> Save parsedData
        | "result" -> SaveResult parsedData.Header
        | _ -> failwithf "unknown type in manual archiving stanza: %s" stanza.Header.Type.Value
    
    let parseManualArchivingStanza ns (elem:XElement) =  // parse something within the "stream"
        parseGenericStanza ns (parseContentManualArchiving) elem
        
    let createManualArchivingStanzaElement (content:ManualArchivingAction) = 
        let col = 
            match content with
            | ManualArchivingAction.Save collection -> collection
            | ManualArchivingAction.SaveResult res -> ChatCollection.FromHeader res // res.CreateAttributes() |> Seq.cast |> Seq.toList

        [ createChatElement col ]
        |> getXElemWithChilds (getXName "save" messageArchiveNs)
        
    let manualArchivingContentGenerator = ContentGenerator.SimpleGenerator createManualArchivingStanzaElement
    
    let createManualArchivingElement (id:string) (target:JabberId option) (data:ManualArchivingAction) = 
        let cType = 
            match data with
            | ManualArchivingAction.Save _ -> "set"
            | ManualArchivingAction.SaveResult _ -> "result"
        Stanza<_>.CreateGen manualArchivingContentGenerator
          { To = target
            From = None
            Id = Some id
            Type = Some cType
            StanzaType = XmlStanzaType.Iq }
          data

    // Automatic Archiving Parsing

    let isContentAutomaticArchiving (stanza:IStanza) = 
        if stanza.Header.StanzaType <> XmlStanzaType.Iq  then false
        else
        let queryItem =
            stanza.Contents.Children 
            |> Seq.tryFind 
                (fun e -> e.Name = getXName "auto" messageArchiveNs)
        queryItem.IsSome 

    let parseContentAutomaticArchiving (stanza:IStanza) =
        if stanza.Header.StanzaType <> XmlStanzaType.Iq then failwith "expected iq stanza"
        else
        let autoItem =
            stanza.Contents.Children 
            |> Seq.find (fun e -> e.Name = getXName "auto" messageArchiveNs)
        let save = 
            autoItem |> forceAttrValue (getXName "save" "") |> (bool.Parse)
        let scope = 
            autoItem |> tryXAttrValue (getXName "scope" "") |> Option.map ArchivingScope.Parse
       
        match stanza.Header.Type.Value with
        | "set" ->  SetAutomaticArchiving (save, scope)
        | _ -> failwithf "unknown type in automatic archiving stanza: %s" stanza.Header.Type.Value
    
    let parseAutomaticArchivingStanza ns (elem:XElement) =  // parse something within the "stream"
        parseGenericStanza ns (parseContentManualArchiving) elem
        
    let createAutomaticArchivingStanzaElement (content:AutomaticArchivingAction) = 
        match content with
        | SetAutomaticArchiving (value, scope) ->
            [
                yield XAttribute(getXName "save" "", StreamData.SimpleBool.ToString value)
                if scope.IsSome then
                    yield XAttribute(getXName "scope" "", scope.Value.AttributeValue)
            ] |> getXElemWithChilds (getXName  "auto" messageArchiveNs) 
        
    let automaticArchivingContentGenerator = ContentGenerator.SimpleGenerator createAutomaticArchivingStanzaElement
    
    let createAutomaticArchivingElement (id:string) (data:AutomaticArchivingAction) = 
        let cType = 
            match data with
            | AutomaticArchivingAction.SetAutomaticArchiving _ -> "set"
        Stanza<_>.CreateGen automaticArchivingContentGenerator
          { To = None
            From = None
            Id = Some id
            Type = Some cType
            StanzaType = XmlStanzaType.Iq }
          data

    // Archive Management Parsing

    let isContentArchiveManagement (stanza:IStanza) = 
        if stanza.Header.StanzaType <> XmlStanzaType.Iq  then false
        else
        let queryItem =
            stanza.Contents.Children 
            |> Seq.tryFind 
                (fun e -> 
                    [ "list"; "retrieve"; "chat"; "remove" ]
                    |> Seq.exists (fun name -> e.Name = getXName name messageArchiveNs))


        queryItem.IsSome 

    let parseContentArchivingManagement (stanza:IStanza) =
        if stanza.Header.StanzaType <> XmlStanzaType.Iq then failwith "expected iq stanza"
        else
        match stanza.Header.Type.Value with
        | "get" ->
            let listItem =
                stanza.Contents.Children 
                |> Seq.tryFind (fun e -> e.Name = getXName "list" messageArchiveNs)
            let retrieveItem =
                stanza.Contents.Children 
                |> Seq.tryFind (fun e -> e.Name = getXName "retrieve" messageArchiveNs)
            if listItem.IsSome then
                MessageArchivingAction.RequestList(listItem.Value |> CollectionFilter.FromAttributes)
            elif retrieveItem.IsSome then
                MessageArchivingAction.RequestCollection(ChatCollectionId.FromAttributes retrieveItem.Value)
            else failwith "Unknown elements in get of archiving element"
        | "result" ->
            let listItem =
                stanza.Contents.Children 
                |> Seq.tryFind (fun e -> e.Name = getXName "list" messageArchiveNs)
            let chatCollection =
                stanza.Contents.Children 
                |> Seq.tryFind (fun e -> e.Name = getXName "chat" messageArchiveNs)
                |> Option.map parseChatElem
            if listItem.IsSome then
                let chatConnections =
                    listItem.Value.Elements()
                    |> Seq.filter (fun e -> e.Name = getXName "chat" messageArchiveNs)
                    |> Seq.map parseChatElem
                    |> Seq.map (fun col -> col.Header)
                    |> Seq.toList
                MessageArchivingAction.RequestResult(chatConnections)
            elif chatCollection.IsSome then
                MessageArchivingAction.CollectionResult chatCollection.Value
            else failwith "Unknown elements in result of archiving element" 
        | "set" ->
            let removeItem =
                stanza.Contents.Children 
                |> Seq.find (fun e -> e.Name = getXName "remove" messageArchiveNs)
            let filter = CollectionFilter.FromAttributes removeItem
            let openAttr = removeItem |> tryXAttrValue (getXName "open" "") |> Option.map bool.Parse
            if openAttr.IsSome && openAttr.Value then
                RemoveOpenCollections (filter.With)
            elif (filter.With.IsSome && filter.Start.IsSome && filter.End.IsNone) then
                RemoveCollection ({ Start = filter.Start.Value; With = filter.With.Value})
            else
                RemoveAllCollections (filter)
        | _ -> failwithf "unknown type in archiving management stanza: %s" stanza.Header.Type.Value

    let parseArchivingManagementStanza ns (elem:XElement) =  // parse something within the "stream"
        parseGenericStanza ns (parseContentManualArchiving) elem
        
    let createArchivingManagementStanzaElement (content:MessageArchivingAction) = 
        let removeHelper (rawFilter:CollectionFilter) (onlyOpen:bool option) = 
            [
                yield! rawFilter.CreateAttributes()
                if onlyOpen.IsSome then
                    yield XAttribute(getXName "open" "", StreamData.SimpleBool.ToString onlyOpen.Value)
            ]
            |> getXElemWithChilds (getXName "remove" messageArchiveNs)
        match content with
        | MessageArchivingAction.RequestList req ->
            req.CreateAttributes()
            |> getXElemWithChilds (getXName "list" messageArchiveNs)
        | MessageArchivingAction.RequestCollection col ->
            col.CreateAttributes()
            |> getXElemWithChilds (getXName "retrieve" messageArchiveNs)
        | MessageArchivingAction.RequestResult res ->
            res
            |> Seq.map (ChatCollection.FromHeader)
            |> Seq.map createChatElement
            |> getXElemWithChilds (getXName "list" messageArchiveNs)
        | MessageArchivingAction.CollectionResult col ->
            createChatElement col
        | MessageArchivingAction.RemoveOpenCollections jid ->
            removeHelper ({With = jid; Start = None; End = None}) (Some true)
        | MessageArchivingAction.RemoveAllCollections filter ->
            let fixedFilter =
                match filter.Start, filter.End with
                | Some _, None ->
                    { filter with End = Some <| DateTime.Parse("2500-01-01T00:00:00Z") }
                | None, Some _ ->
                    { filter with Start = Some <| DateTime.Parse("0000-01-01T00:00:00Z")}
                | _ -> filter
                    
            removeHelper fixedFilter None
        | MessageArchivingAction.RemoveCollection id ->
            removeHelper ({With = Some id.With; Start = Some id.Start; End = None}) None

    let archivingManagementContentGenerator = ContentGenerator.SimpleGenerator createArchivingManagementStanzaElement
    
    let createArchivingManagementElement (id:string) (target:JabberId option) (data:MessageArchivingAction) = 
        let cType = 
            match data with
            | MessageArchivingAction.RequestList _ 
            | MessageArchivingAction.RequestCollection _ -> "get"
            | MessageArchivingAction.RequestResult _
            | MessageArchivingAction.CollectionResult _ ->  "result"
            | MessageArchivingAction.RemoveOpenCollections _
            | MessageArchivingAction.RemoveAllCollections _
            | MessageArchivingAction.RemoveCollection _ -> "set"
        Stanza<_>.CreateGen archivingManagementContentGenerator
          { To = target
            From = None
            Id = Some id
            Type = Some cType
            StanzaType = XmlStanzaType.Iq }
          data

    // Replication Parsing

    let isContentReplication (stanza:IStanza) = 
        if stanza.Header.StanzaType <> XmlStanzaType.Iq  then false
        else
        let queryItem =
            stanza.Contents.Children 
            |> Seq.tryFind 
                (fun e -> 
                    e.Name = getXName "modified" messageArchiveNs)
        queryItem.IsSome 

    let parseContentReplication (stanza:IStanza) =
        if stanza.Header.StanzaType <> XmlStanzaType.Iq then failwith "expected iq stanza"
        else
        let parseChangeItem (e:XElement) =
            let t =
                match e.Name.LocalName, e.Name.NamespaceName with
                | "changed", Equals messageArchiveNs ->
                    ChangeItemType.Changed
                | "removed", Equals messageArchiveNs ->                
                    ChangeItemType.Removed
                | _ -> failwith "unknown ChangeItem item"
            { Type = t; Data = ChangeItemData.FromAttributes e}
        let modifiedElem =
            stanza.Contents.Children 
            |> Seq.find (fun e -> e.Name = getXName "modified" messageArchiveNs)
        match stanza.Header.Type.Value with
        | "get" ->
            let startAttr = 
                modifiedElem |> forceAttrValue (getXName "start" "") |> StreamData.DateTime.Parse
            ReplicationAction.RequestModifiedSince startAttr
        | "result" ->
            let validChangeItems = [ "changed"; "removed" ]
            let changeItems =
                modifiedElem.Elements()
                |> Seq.filter (fun e -> validChangeItems |> Seq.exists (fun name -> e.Name = getXName name messageArchiveNs))
                |> Seq.map parseChangeItem
                |> Seq.toList
            ReplicationAction.RequestModifiedResult changeItems
        | _ -> failwithf "unknown type in automatic archiving stanza: %s" stanza.Header.Type.Value
    
    let parseReplicationStanza ns (elem:XElement) =  // parse something within the "stream"
        parseGenericStanza ns (parseContentReplication) elem
        
    let createReplicationStanzaElement (content:ReplicationAction) = 
        let createChangeItemElement (changeItem : ChangeItem) = 
            let name = 
                match changeItem.Type with
                | ChangeItemType.Changed -> "changed"
                | ChangeItemType.Removed -> "removed"
            changeItem.Data.CreateAttributes()
            |> getXElemWithChilds (getXName name messageArchiveNs)

        match content with
        | ReplicationAction.RequestModifiedSince date ->
            [ XAttribute(getXName "start" "", date |> StreamData.DateTime.ToString) ]
            |> getXElemWithChilds (getXName "modified" messageArchiveNs)
        | ReplicationAction.RequestModifiedResult res ->
            res
            |> Seq.map createChangeItemElement
            |> getXElemWithChilds (getXName "modified" messageArchiveNs)

    let replicationContentGenerator = ContentGenerator.SimpleGenerator createReplicationStanzaElement
    
    let createReplicationElement (id:string) (target:JabberId option) (data:ReplicationAction) = 
        let cType = 
            match data with
            | ReplicationAction.RequestModifiedSince _ -> "get"
            | ReplicationAction.RequestModifiedResult _ -> "result"
        Stanza<_>.CreateGen replicationContentGenerator
          { To = target
            From = None
            Id = Some id
            Type = Some cType
            StanzaType = XmlStanzaType.Iq }
          data