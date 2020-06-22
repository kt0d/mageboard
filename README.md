A simple, javascriptless imageboard written in Haskell, with SQLite3 as the backend.

## Features
* Captcha
* Upload files: 

| Format | Thumbnailing |
| --- | --- |
| JPG | ✅ |
| PNG | ✅ |
| GIF | ✅ |
| WEBM | ✅ |
| MP4 | ✅ |
| MP3 | ❌ |
| OGG | ❌ |
| PDF | ✅ |
| EPUB | ❌ |
| SWF | ❌ |

* Moderation:
  * Delete posts
  * Delete files
  * Unlink files from posts
  * Toggle thread configuration:
    * Locked
    * Sticky
    * Cycle
    * Autosage
* Admin panel:
  * Configure existing boards 
  * Add new boards
  * Add moderator accounts
* [Alternate CSS stylesheet](https://developer.mozilla.org/en-US/docs/Web/CSS/Alternative_style_sheets)

### Missing features
* Cross-thread and cross-board linking (>> and >>>)
* Logging of moderation actions

## Build

Non-Haskell build-time dependencies:
* `libpcre2-16.so` 
* `pcre2.h`

Run-time dependencies:
* `libpcre2-16.so` 
* `ffmpeg` (with `ffprobe`)
* `GraphicsMagick`

## Credits

Pictures present in `static` directory are licensed under CC0.

Pictures present in `static/styles/img/` are taken from BRChan.

Databse schema is slightly modified cut-down version of Picochan schema.
