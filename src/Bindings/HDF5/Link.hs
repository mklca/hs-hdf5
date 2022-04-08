{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.HDF5.Link
    ( createHardLink
    , createSoftLink
    , createExternalLink

    , doesLinkExist

    , moveLink
    , copyLink
    , deleteLink

    , LinkType(..)
    , LinkInfo1(..)
    , getLinkInfo1

    , getSymLinkVal

    , iterateLinks1
    , iterateLinksByName1

    , visitLinks1
    , visitLinksByName1
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.Datatype.Internal
import Bindings.HDF5.Error
import Bindings.HDF5.Group
import Bindings.HDF5.PropertyList.LAPL
import Bindings.HDF5.PropertyList.LCPL
import Bindings.HDF5.Raw.H5
import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5L
import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Raw.Util
import Control.Exception (SomeException, try, finally, throwIO)
import qualified Data.ByteString as BS
import Data.IORef
import Foreign
import Foreign.C
import Foreign.Ptr.Conventions

{-# ANN module "HLint: ignore Use camelCase" #-}

createHardLink :: (Location src, Location dst) => src -> BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
createHardLink src srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \csrcName ->
            BS.useAsCString dstName $ \cdstName ->
                h5l_create_hard (hid src) csrcName (hid dst) cdstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

createSoftLink :: Location dst => BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
createSoftLink srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \csrcName ->
            BS.useAsCString dstName $ \cdstName ->
                h5l_create_soft csrcName (hid dst) cdstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

createExternalLink :: Location loc => BS.ByteString -> BS.ByteString -> loc -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
createExternalLink file obj loc name lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString file $ \cfile ->
            BS.useAsCString obj $ \cobj ->
                BS.useAsCString name $ \cname ->
                    h5l_create_external cfile cobj (hid loc) cname (maybe h5p_DEFAULT hid lcpl) (maybe h5p_DEFAULT hid lapl)

doesLinkExist :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO Bool
doesLinkExist loc name lapl =
    htriToBool $
        BS.useAsCString name $ \cname ->
            h5l_exists (hid loc) cname (maybe h5p_DEFAULT hid lapl)

moveLink :: (Location src, Location dst) => src -> BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
moveLink  src srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \csrcName ->
            BS.useAsCString dstName $ \cdstName ->
                h5l_move (hid src) csrcName (hid dst) cdstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

copyLink :: (Location src, Location dst) => src -> BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
copyLink  src srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \csrcName ->
            BS.useAsCString dstName $ \cdstName ->
                h5l_copy (hid src) csrcName (hid dst) cdstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

deleteLink :: Location t => t -> BS.ByteString -> Maybe LAPL -> IO ()
deleteLink loc name lapl =
    withErrorCheck_ $
        BS.useAsCString name $ \cname ->
            h5l_delete (hid loc) cname (maybe h5p_DEFAULT hid lapl)

data LinkType
    = External
    | Hard
    | Soft
    | OtherLinkType !H5L_type_t
    deriving (Eq, Ord, Read, Show)

linkTypeFromCode :: H5L_type_t -> LinkType
linkTypeFromCode c
    | c == h5l_TYPE_EXTERNAL    = External
    | c == h5l_TYPE_HARD        = Hard
    | c == h5l_TYPE_SOFT        = Soft
    | c >= h5l_TYPE_UD_MIN      = OtherLinkType c
    | otherwise                 = error ("Unknown link type: " ++ show c)


data LinkInfo1 = LinkInfo1
    { linkType          :: LinkType
    , linkCOrderValid   :: Bool
    , linkCOrder        :: Int64
    , linkCSet          :: CSet
    , linkAddress       :: HAddr
    , linkValSize       :: CSize
    } deriving (Eq, Ord, Read, Show)

readLinkInfo1 :: H5L_info1_t -> LinkInfo1
readLinkInfo1 i  = LinkInfo1
    { linkType          = linkTypeFromCode (h5l_info1_t'type i)
    , linkCOrderValid   = hboolToBool (h5l_info1_t'corder_valid i)
    , linkCOrder        = h5l_info1_t'corder i
    , linkCSet          = cSetFromCode (h5l_info1_t'cset i)
    , linkAddress       = HAddr (h5l_info1_t'u'address i)
    , linkValSize       = h5l_info1_t'u'val_size i
    }

getLinkInfo1 :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO LinkInfo1
getLinkInfo1 loc name lapl =
    fmap readLinkInfo1 $
        withOut_ $ \info ->
            withErrorCheck_ $
                BS.useAsCString name $ \cname ->
                    h5l_get_info1 (hid loc) cname info (maybe h5p_DEFAULT hid lapl)

getSymLinkVal :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO BS.ByteString
getSymLinkVal loc name mb_lapl =
    BS.useAsCString name $ \cname -> do
        let lapl = maybe h5p_DEFAULT hid mb_lapl
        info <- withOut_ $ \info ->
            withErrorCheck_ $
                    h5l_get_info1 (hid loc) cname info lapl

        let n = h5l_info1_t'u'val_size info

        buf <- mallocBytes (fromIntegral n)

        withErrorCheck_ $
            h5l_get_val (hid loc) cname (OutArray buf) n lapl
        -- TODO: this will leak memory if an exception is thrown

        BS.packCStringLen (buf, fromIntegral n)


foreign import ccall "wrapper" wrap_H5L_iterate1_t
    :: (HId_t -> CString -> In H5L_info1_t -> InOut a -> IO HErr_t)
    -> IO (FunPtr (HId_t -> CString -> In H5L_info1_t -> InOut a -> IO HErr_t))

with_iterate1_t :: (Group -> BS.ByteString -> LinkInfo1 -> IO HErr_t)
     -> (H5L_iterate1_t () -> InOut () -> IO HErr_t)
     -> IO HErr_t
with_iterate1_t op f = do
    exception1 <- newIORef Nothing :: IO (IORef (Maybe SomeException))

    op1 <- wrap_H5L_iterate1_t $ \grp name (In link) _opData -> do
        name1 <- BS.packCString name
        link1 <- peek link
        result <- try (op (uncheckedFromHId grp) name1 (readLinkInfo1 link1))
        case result of
            Left exc -> do
                writeIORef exception1 (Just exc)
                return maxBound
            Right x -> return x

    result <- f op1 (InOut nullPtr) `finally` freeHaskellFunPtr op1

    if result == maxBound
        then do
            exception2 <- readIORef exception1
            maybe (return result) throwIO exception2

        else return result

iterateLinks1 :: Location t => t -> IndexType -> IterOrder -> Maybe HSize -> (Group -> BS.ByteString -> LinkInfo1 -> IO HErr_t) -> IO HSize
iterateLinks1 loc indexType order startIndex op =
    fmap HSize $
        withInOut_ (maybe 0 hSize startIndex) $ \ioStartIndex ->
            withErrorCheck_ $
                with_iterate1_t op $ \iop opData ->
                    h5l_iterate (hid loc) (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData

iterateLinksByName1 :: Location t => t -> BS.ByteString -> IndexType -> IterOrder -> Maybe HSize -> Maybe LAPL -> (Group -> BS.ByteString -> LinkInfo1 -> IO HErr_t) -> IO HSize
iterateLinksByName1 loc groupName indexType order startIndex lapl op =
    fmap HSize $
        withInOut_ (maybe 0 hSize startIndex) $ \ioStartIndex ->
            withErrorCheck_ $
                with_iterate1_t op $ \iop opData ->
                    BS.useAsCString groupName $ \cgroupName ->
                        h5l_iterate_by_name (hid loc) cgroupName (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData (maybe h5p_DEFAULT hid lapl)

visitLinks1 :: Location t => t -> IndexType -> IterOrder -> (Group -> BS.ByteString -> LinkInfo1 -> IO HErr_t) -> IO ()
visitLinks1 loc indexType order op =
    withErrorCheck_ $
        with_iterate1_t op $ \iop opData ->
            h5l_visit (hid loc) (indexTypeCode indexType) (iterOrderCode order) iop opData

visitLinksByName1 :: Location t => t -> BS.ByteString -> IndexType -> IterOrder -> Maybe LAPL -> (Group -> BS.ByteString -> LinkInfo1 -> IO HErr_t) -> IO ()
visitLinksByName1 loc groupName indexType order lapl op =
    withErrorCheck_ $
        with_iterate1_t op $ \iop opData ->
            BS.useAsCString groupName $ \cgroupName ->
                h5l_visit_by_name (hid loc) cgroupName (indexTypeCode indexType) (iterOrderCode order) iop opData (maybe h5p_DEFAULT hid lapl)
