-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2011 Peter Althainz
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- Network.hs

-- |Base API, Network Module.


module HGamer3D.APIs.Base.Network.Network

(
		NetworkClient (..),
		NetworkServer (..),
		NetworkPacket (..),
		createNetworkClient,
		createNetworkServer,
		connectClientToServer,
		disconnectClient,
		NetworkNode (..)
)

where

import GHC.Ptr

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector

import Foreign.Ptr
import HGamer3D.Bindings.Enet.ClassEnet as Enet
import HGamer3D.Bindings.Enet.ClassEnetServer as EnetServer
import HGamer3D.Bindings.Enet.ClassEnetClient as EnetClient
import HGamer3D.Bindings.Enet.ClassEnetPacket as EnetPacket
import HGamer3D.Data.HG3DClass

import HGamer3D.APIs.Base.Engine.Types

import Control.Monad.Trans
import Control.Monad.Reader

data NetworkClient = NetworkClient HG3DClass
data NetworkServer = NetworkServer HG3DClass
data NetworkPacket = NetworkPacket {
				clientname :: String,
				channel :: Int,
				message :: String 
				}

createNetworkClient :: MHGamer3D NetworkClient
createNetworkClient = do
	client <- liftIO $ Enet.createClient
	return (NetworkClient client)
	
createNetworkServer :: Int -> MHGamer3D NetworkServer
createNetworkServer port = do
	server <- liftIO $ Enet.createServer port
	return (NetworkServer server)

connectClientToServer :: NetworkClient -> String -> Int -> MHGamer3D Bool
connectClientToServer (NetworkClient client) serveraddress port = do
	ok <- liftIO $ EnetClient.connect client serveraddress port
	return ok
	
disconnectClient :: NetworkClient -> MHGamer3D Bool
disconnectClient (NetworkClient client) = do
	ok <- liftIO $ EnetClient.disconnect client
	return ok

class NetworkNode a where
	sendNetworkMessage :: a -> String -> Int -> String -> MHGamer3D ()
	receiveNetworkMessages :: a -> TimeMS -> MHGamer3D [NetworkPacket]


_transformPackage :: HG3DClass -> MHGamer3D NetworkPacket
_transformPackage p = do
	message <- liftIO $ EnetPacket.getData p
	channel <- liftIO $ EnetPacket.getChannel p
	clientname <- liftIO $ EnetPacket.getPeer p
	liftIO $ EnetPacket.delete p
	return $ NetworkPacket clientname channel message

_receiveClient client ms pkg = do
	liftIO $ EnetClient.serve client ms
	p <- liftIO $ EnetClient.getPacket client
	let (HG3DClass aptr bptr) = p
	if aptr /= nullPtr 
		then do
			p' <- _transformPackage p
			pkg' <- _receiveClient client ms (pkg ++ [p'])
			return pkg'
		else do
			return pkg 
	
_receiveServer server ms pkg = do
	liftIO $ EnetServer.serve server ms
	p <- liftIO $ EnetServer.getPacket server
	let (HG3DClass aptr bptr) = p
	if aptr /= nullPtr 
		then do
			p' <- _transformPackage p
			pkg' <- _receiveServer server ms (pkg ++ [p'])
			return pkg'
		else do
			return pkg 
	
instance NetworkNode NetworkClient where
	
	sendNetworkMessage (NetworkClient client) clientname channel message = do
		liftIO $ EnetClient.send client message channel
		return ()

	receiveNetworkMessages (NetworkClient client) (TimeMS ms) = do
		allPs <- _receiveClient client ms []
		return allPs

instance NetworkNode NetworkServer where

	sendNetworkMessage (NetworkServer server) clientname channel message = do
		liftIO $ EnetServer.send server clientname message channel
		return ()

	receiveNetworkMessages (NetworkServer server) (TimeMS ms) = do
		allPs <- _receiveServer server ms []
		return allPs


	

