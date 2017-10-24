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
import Bindings.HDF5.Raw.H5O
import Bindings.HDF5.Raw.H5T
import Bindings.HDF5.Raw.Util
import Foreign.Ptr.Conventions
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


openAttribute :: Location loc => loc -> BS.ByteString -> IO Attribute
openAttribute loc name =
  fmap Attribute $ withErrorCheck $ BS.useAsCString name $ \cname ->
    -- AAPL is not currently used
    h5a_open (hid loc) cname h5p_DEFAULT

closeAttribute :: Attribute -> IO ()
closeAttribute (Attribute attr_id) = withErrorCheck_ $ h5a_close attr_id

readAttribute = undefined
writeAttribute = undefined
iterateAttributes = undefined

deleteAttribute :: Location loc => loc -> BS.ByteString -> IO ()
deleteAttribute loc name = withErrorCheck_ $ BS.useAsCString name $ \cname ->
  h5a_delete (hid loc) cname


{-
typedef struct {
    hbool_t             corder_valid;
    H5O_msg_crt_idx_t   corder;
    H5T_cset_t          cset;
    hsize_t             data_size;
} H5A_info_t;
-}

newtype H5O_msg_crt_idx = H5O_msg_crt_idx H5O_msg_crt_idx_t
newtype H5T_cset = H5T_cset H5T_cset_t

data AttributeInfo = AttributeInfo
  { attrCOrderValid :: !Bool
  , attrCOrder      :: !H5O_msg_crt_idx
  , attrCSet        :: !H5T_cset
  , attrDataSize    :: !HSize
  }

-- TODO : higher-level representation of AttributeInfo contents
readAttributeInfo :: H5A_info_t -> AttributeInfo
readAttributeInfo (H5A_info_t a b c d) =
  AttributeInfo (hboolToBool a) (H5O_msg_crt_idx b) (H5T_cset c) (HSize d)

getAttributeInfo :: Attribute -> IO AttributeInfo
getAttributeInfo (Attribute attr_id) =
  fmap readAttributeInfo $
    withOut_ $ \info -> withErrorCheck_ $
                          h5a_get_info attr_id info
