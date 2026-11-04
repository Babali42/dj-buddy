module CLI.Render where

import App (ResolvedPlaylist, ResolvedPlaylistMember (..), ResolvedTransition (..))
import Data.Text (Text)
import Data.Text qualified as T
import Domain.MusicLibrary (TestedTransition (..), Track, TrackId)
import Domain.Playlist (PlaylistMember (..))
import Domain.Transitions (Transition (..))
import Fmt (Builder, fmt, unlinesF, (+|), (+||), (|+), (||+))

renderMusicLibrary :: [(TrackId, Track)] -> Text
renderMusicLibrary lib =
  fmt $ unlinesF $ map formatTrack lib
  where
    formatTrack :: (TrackId, Track) -> Builder
    formatTrack (trackId, track) =
      "" +| trackId |+ ": " +|| track ||+ ""

renderTransition :: Transition -> Text
renderTransition (Tested i _) =
  fmt $ "Tested (notes: " +| notes |+ ")"
  where
    notes = case i.notes of
      Nothing -> "NA"
      Just msg -> msg
renderTransition (Harmonic t _) = fmt $ "Harmonic: " +|| t ||+ ""

renderPlaylistMember :: PlaylistMember -> Text
renderPlaylistMember (StartingTrack _) = "start"
renderPlaylistMember (MiddleTrack t) = renderTransition t

renderResolvedPlaylistMember :: ResolvedPlaylistMember -> Text
renderResolvedPlaylistMember pm =
  fmt $
    "" +|| pm.track ||+ " - " +| renderPlaylistMember (pm.playlistMember) |+ ""

renderResolvedPlaylist :: ResolvedPlaylist -> Text
renderResolvedPlaylist =
  T.unlines . map renderResolvedPlaylistMember

renderResolvedTransition :: ResolvedTransition -> Text
renderResolvedTransition rt =
  fmt $ "" +|| rt.track ||+ " (" +| renderTransition rt.transition |+ ")"

renderResolvedTransitions :: [ResolvedTransition] -> Text
renderResolvedTransitions = T.unlines . map renderResolvedTransition
