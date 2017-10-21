{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Typesafe wrappers around HDF5 functinos for the H5A API.

Feature coverage:

  h5a_get_name                  	[ FAIL ]
  h5a_get_type                  	[ FAIL ]
  h5a_read                      	[ FAIL ]
  h5a_open                      	[ FAIL ]
  h5a_get_info                  	[ FAIL ]
  h5a_get_num_attrs             	[ FAIL ]
  h5a_open_by_idx               	[ FAIL ]
  h5a_get_info_by_idx           	[ FAIL ]
  h5a_create_by_name            	[ FAIL ]
  h5a_exists_by_name            	[ FAIL ]
  h5a_delete_by_idx             	[ FAIL ]
  h5a_rename                    	[ FAIL ]
  h5a_write                     	[ FAIL ]
  h5a_iterate1                  	[ FAIL ]
  h5a_delete                    	[ FAIL ]
  h5a_open_name                 	[ FAIL ]
  h5a_get_info_by_name          	[ FAIL ]
  h5a_get_space                 	[ FAIL ]
  h5a_rename_by_name            	[ FAIL ]
  h5a_iterate_by_name           	[ FAIL ]
  h5a_iterate2                  	[ FAIL ]
  h5a_get_name_by_idx           	[ FAIL ]
  h5a_get_storage_size          	[ FAIL ]
  h5a_open_by_name              	[ FAIL ]
  h5a_create2                   	[ FAIL ]
  h5a_get_create_plist          	[ FAIL ]
  h5a_open_idx                  	[ FAIL ]
  h5a_close                     	[ FAIL ]
  h5a_create1                   	[ FAIL ] (deprecated)
  h5a_delete_by_name            	[ FAIL ]
  h5a_exists                    	[  OK  ]

-}
module Bindings.HDF5.Attribute
       ( doesAttributeExist
       , createAttribute
       , openAttribute
       , closeAttribute
       , readAttribute
       , iterateAttributes
       , deleteAttribute
       , getAttributeInfo
       ) where

import qualified Data.ByteString as BS
import Data.Tagged

import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.Raw.H5A
import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Object
import Bindings.HDF5.Datatype.Internal
import Bindings.HDF5.Dataspace

-- * The Attribute type

newtype Attribute = Attribute HId_t
                    deriving (Eq, HId, FromHId, HDFResultType)

instance Object Attribute where
  staticObjectType = Tagged (Just AttrObj)

-- * Attribute functions

doesAttributeExist :: Location loc => loc -> BS.ByteString -> IO Bool
doesAttributeExist loc name =
  htriToBool $ BS.useAsCString name $ \cname -> h5a_exists (hid loc) cname


createAttribute :: Location loc =>
                   loc
                   -> BS.ByteString
                   -> Datatype
                   -> Dataspace
                   -> IO Attribute
createAttribute loc name dtype dspace =
  fmap Attribute $ withErrorCheck $ BS.useAsCString name $ \cname ->
    -- ACPL and AAPL are not currently used
    h5a_create2 (hid loc) cname (hid dtype) (hid dspace) h5p_DEFAULT h5p_DEFAULT


openAttribute = undefined
closeAttribute = undefined
readAttribute = undefined
iterateAttributes = undefined
deleteAttribute = undefined
getAttributeInfo = undefined
