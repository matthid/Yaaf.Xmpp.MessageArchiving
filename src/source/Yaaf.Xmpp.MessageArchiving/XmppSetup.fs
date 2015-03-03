// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.IM.MessageArchiving

open Yaaf.Helper

open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.IM

open Yaaf.Logging
open Yaaf.Xmpp.ServiceDiscovery
open Yaaf.Xmpp.MessageArchiving

#if CSHARP_EXTENSIONS
[<System.Runtime.CompilerServices.Extension>]
#endif
module XmppSetup =
    let setupArchivingCoreClient (runtime:XmppRuntime) =
        let mgr = runtime.PluginManager
        mgr.RegisterPlugin<MessageArchivingClientPlugin>()

    /// Adds MessagePlugin with all dependencies (besides CoreClient and Disco) 
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let AddMessageArchivingClientCore (setup: ClientSetup) = 
        setup
        |> XmppSetup.addHelper ignore setupArchivingCoreClient
    let addMessagingClientCore setup = AddMessageArchivingClientCore(setup)
    
    /// Adds MessagePlugin with all dependencies
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let AddMessageArchivingClient (setup: ClientSetup) = 
        setup
        |> XmppSetup.addMessagingClientCore
        |> addMessagingClientCore
    let addMessageArchivingClient setup = AddMessageArchivingClient(setup)

namespace Yaaf.Xmpp.MessageArchiving.Server

open Yaaf.Xmpp
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Runtime.Features
open Yaaf.Xmpp.IM
open Yaaf.Xmpp.IM.Server

#if CSHARP_EXTENSIONS
[<System.Runtime.CompilerServices.Extension>]
#endif
module XmppSetup =

    let setupArchivingCoreServer (runtime:XmppRuntime) =
        let mgr = runtime.PluginManager
        mgr.RegisterPlugin<MessageArchivingServerPlugin>()

    /// Sets the given IBindConfig to the current configuration
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let internal SetMessageArchivingServerConfig (setup, config:IMessageArchivingServerPluginConfig) =
        XmppSetup.SetConfig<MessageArchivingServerPluginConfig,_> (setup, config)
    let internal setMessageArchivingServerConfig config setup = SetMessageArchivingServerConfig(setup, config)

    /// Adds MessagePlugin with all dependencies (besides CoreClient and Disco) 
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let AddMessageArchivingServerCore (setup: ClientSetup, config) = 
        setup
        |> setMessageArchivingServerConfig config
        |> XmppSetup.addHelper ignore setupArchivingCoreServer
    let addMessageArchivingServerCore config setup = AddMessageArchivingServerCore(setup, config)
    
    
#if CSHARP_EXTENSIONS
[<System.Runtime.CompilerServices.Extension>]
#endif
module XmppServerSetup =
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let addMessageArchivingPlugin config setup =
        setup
        |> XmppServerSetup.addToAllStreams (XmppSetup.addMessageArchivingServerCore config)