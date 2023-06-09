(defpackage #:org.shirakumo.fraf.opus.cffi
  (:use #:cl)
  (:shadow #:close #:format #:error #:read)
  (:export
   #:libopus
   #:error
   #:picture-format
   #:picture-type
   #:request-option
   #:format
   #:gain-type
   #:whence
   #:head
   #:head-version
   #:head-channel-count
   #:head-pre-skip
   #:head-input-sample-rate
   #:head-output-gain
   #:head-mapping-family
   #:head-stream-count
   #:head-coupled-count
   #:head-mapping
   #:tags
   #:tags-user-comments
   #:tags-comment-lengths
   #:tags-comments
   #:tags-vendor
   #:picture-tag
   #:picture-tag-type
   #:picture-tag-mime-type
   #:picture-tag-description
   #:picture-tag-width
   #:picture-tag-height
   #:picture-tag-depth
   #:picture-tag-colors
   #:picture-tag-ata-length
   #:picture-tag-data
   #:picture-tag-format
   #:server-info
   #:server-info-name
   #:server-info-description
   #:server-info-genre
   #:server-info-url
   #:server-info-server
   #:server-info-content-type
   #:server-info-bitrate-kbps
   #:server-info-public-p
   #:server-info-ssl-p
   #:callbacks
   #:callbacks-read
   #:callbacks-seek
   #:callbacks-tell
   #:callbacks-close
   #:head-parse
   #:granule-sample
   #:tags-parse
   #:tags-copy
   #:tags-init
   #:tags-add
   #:tags-add-comment
   #:tags-set-binary-suffix
   #:tags-query
   #:tags-query-count
   #:tags-get-binary-suffix
   #:tags-get-album-gain
   #:tags-get-track-gain
   #:tags-clear
   #:tags-compare
   #:tags-n-compare
   #:picture-tag-parse
   #:picture-tag-init
   #:picture-tag-clear
   #:server-info-init
   #:server-info-clear
   #:fopen
   #:fdopen
   #:freopen
   #:mem-stream-create
   #:test
   #:open-file
   #:open-memory
   #:open-callbacks
   #:open-stream
   #:test-file
   #:test-memory
   #:test-callbacks
   #:test-open
   #:free
   #:seekable-p
   #:link-count
   #:serial-number
   #:channel-count
   #:raw-total
   #:pcm-total
   #:head
   #:tags
   #:current-link
   #:bitrate
   #:bitrate-instant
   #:raw-tell
   #:pcm-tell
   #:raw-seek
   #:pcm-seek
   #:set-decode-callback
   #:set-gain-offset
   #:set-dither-enabled
   #:read
   #:read-float
   #:read-stereo
   #:read-stereo-float))

(defpackage #:org.shirakumo.fraf.opus
  (:use #:cl)
  (:local-nicknames
   (#:opus #:org.shirakumo.fraf.opus.cffi))
  (:shadow #:open #:close #:decode-float)
  (:export
   #:opus-error
   #:file
   #:code
   #:init
   #:shutdown
   #:check-return
   #:file
   #:handle
   #:channels
   #:sample-count
   #:read-fun
   #:seek-fun
   #:index-fun
   #:close-fun
   #:close
   #:open-file
   #:open-pointer
   #:open-vector
   #:open-callback
   #:open
   #:with-file
   #:samplerate
   #:duration
   #:seekable-p
   #:link-count
   #:serial-number
   #:current-link
   #:bitrate
   #:instant-bitrate
   #:index
   #:seek
   #:comments
   #:vendor
   #:gain
   #:decode-float
   #:decode-int16))
