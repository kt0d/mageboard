A simple anonymous javascriptless imageboard written in Haskell, with SQLite3 as the backend.

[Haddock-generated documentation](https://kt0d.github.io/mageboard/)

## Features
* Boards with catalog
* *Recent posts* page
* Post markup
* Captcha
* File upload: 

| Format | Thumbnailing |
| --- | --- |
| JPG   | ✅ |
| PNG   | ✅ |
| GIF   | ✅ |
| WEBM  | ✅ |
| MP4   | ✅ |
| MP3   | ❌ |
| OGG   | ❌ |
| PDF   | ✅ |
| EPUB  | ❌ |
| SWF   | ❌ |

* Thread flags
  * Locked
  * Sticky
  * Cycle
  * Autosage
* Moderation:
  * Delete posts
  * Delete files
  * Unlink files from posts
  * Toggle thread configuration:
  * Change password to your account
* Admin panel:
  * Configure existing boards 
  * Add new boards
  * Add moderator accounts
* [Alternate CSS stylesheet](https://developer.mozilla.org/en-US/docs/Web/CSS/Alternative_style_sheets)

### Missing features / TODO
* Cross-thread and cross-board linking (>> and >>>)
* Logging of moderation actions
* Moving thread between boards
* Ability for moderators to sign their posts
* File-based configuration
* Performance improvements:
  * One DB connection per request
* Replace hs-captcha

## Build

Non-Haskell build-time dependencies:
* `libpcre2-16.so` 
* `pcre2.h`

Run-time dependencies:
* `libpcre2-16.so` 
* `ffmpeg` (with `ffprobe`)
* `GraphicsMagick`

hs-captcha also has dependency on gd package, which depends on gd library.

## Credits

Pictures present in `static` directory are licensed under CC0.

Pictures present in `static/styles/img/` are taken from BRChan.

Databse schema is slightly modified, cut-down version of Picochan schema.
