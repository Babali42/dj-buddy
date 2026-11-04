module CLI.Render where

import App (ResolvedPlaylist, ResolvedPlaylistMember (..), ResolvedTransition (..))
import Data.Text (Text)
import Domain.MusicLibrary (TestedTransition (..), Track, TrackId)
import Domain.Playlist (PlaylistMember (..))
import Domain.Transitions (Transition (..))
import Fmt (Builder, fmt, unlinesF, (+|), (|+))

renderMusicLibrary :: [(TrackId, Track)] -> Text
renderMusicLibrary lib =
  fmt $ unlinesF $ map formatTrack lib
  where
    formatTrack :: (TrackId, Track) -> Builder
    formatTrack (trackId, track) =
      "" +| trackId |+ ": " +| show track |+ ""

renderTransition :: Transition -> String
renderTransition (Tested i _) = "Tested (notes: " ++ notes ++ ")"
  where
    notes = case i.notes of
      Nothing -> "NA"
      Just msg -> msg
renderTransition (Harmonic t _) = "Harmonic: " ++ show t

renderPlaylistMember :: PlaylistMember -> String
renderPlaylistMember (StartingTrack _) = "start"
renderPlaylistMember (MiddleTrack t) = renderTransition t

renderResolvedPlaylistMember :: ResolvedPlaylistMember -> String
renderResolvedPlaylistMember pm =
  show (pm.track) ++ " - " ++ renderPlaylistMember (pm.playlistMember)

renderResolvedPlaylist :: ResolvedPlaylist -> String
renderResolvedPlaylist =
  unlines . map renderResolvedPlaylistMember

renderResolvedTransition :: ResolvedTransition -> String
renderResolvedTransition rt = show rt.track ++ " (" ++ renderTransition rt.transition ++ ")"

renderResolvedTransitions :: [ResolvedTransition] -> String
renderResolvedTransitions = unlines . map renderResolvedTransition
