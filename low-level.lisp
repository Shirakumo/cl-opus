#|
 This file is a part of cl-opus
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.opus.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(cffi:define-foreign-library libopus
  (:darwin (:or #+X86 "libopus-mac-i686.dylib"
                #+X86-64 "libopus-mac-amd64.dylib"
                #+ARM64 "libopus-mac-arm64.dylib"))
  (:unix (:or #+X86 "libopus-lin-i686.so"
              #+X86-64 "libopus-lin-amd64.so"))
  (:windows (:or #+X86 "libopus-win-i686.dll"
                 #+X86-64 "libopus-win-amd64.dll")))

(cffi:defcenum (error :int :allow-undeclared-values T)
  (:ok 0)
  (:false -1)
  (:end-of-file -2)
  (:page-sequence-hole -3)
  (:read-failed -128)
  (:internal-error -129)
  (:not-implemented -130)
  (:invalid-argument -131)
  (:invalid-format -132)
  (:bad-header -133)
  (:bad-version -134)
  (:no-audio-signal -135)
  (:bad-packet -136)
  (:bad-link -137)
  (:cannot-seek -138)
  (:bad-timestamp -139))

(cffi:defcenum picture-format
  (:unknown -1)
  (:url 0)
  (:jpeg 1)
  (:png 2)
  (:gif 3))

(cffi:defcenum picture-type
  (:other 0)
  :file-icon-png
  :file-icon
  :front-cover
  :back-cover
  :leaflet
  :media
  :lead
  :artist
  :conductor
  :band
  :composer
  :lyricist
  :location
  :during-recording
  :during-performance
  :screen-capture
  :bright-colored-fish
  :illustration
  :band-logotype
  :publisher-logotype)

(cffi:defcenum request-option
  (:skip-ssl-certificate-check 6464)
  (:http-proxy-host 6528)
  (:http-proxy-port 6592)
  (:http-proxy-user 6656)
  (:http-proxy-pass 6720)
  (:get-server-info 6784))

(cffi:defcenum format
  (:short 7008)
  (:float 7040)
  (:default 6720))

(cffi:defcenum gain-type
  (:header 0)
  (:album 3007)
  (:track 3008)
  (:absolute 3009))

(cffi:defcenum whence
  (:start 0)
  (:current 1)
  (:end 2))

(cffi:defcstruct (head :conc-name head-)
  (version :int)
  (channel-count :int)
  (pre-skip :uint)
  (input-sample-rate :uint32)
  (output-gain :int)
  (mapping-family :int)
  (stream-count :int)
  (coupled-count :int)
  (mapping :uchar :count 255))

(cffi:defcstruct (tags :conc-name tags-)
  (user-comments :pointer)
  (comment-lengths :pointer)
  (comments :int)
  (vendor :string))

(cffi:defcstruct (picture-tag :conc-name picture-tag-)
  (type :int32)
  (mime-type :string)
  (description :string)
  (width :uint32)
  (height :uint32)
  (depth :uint32)
  (colors :uint32)
  (data-length :uint32)
  (data :pointer)
  (format :int))

(cffi:defcstruct (server-info :conc-name server-info-)
  (name :string)
  (description :string)
  (genre :string)
  (url :string)
  (server :string)
  (content-type :string)
  (bitrate-kbps :int32)
  (public-p :int)
  (ssl-p :int))

(cffi:defcstruct (callbacks :conc-name callbacks-)
  (read :pointer)
  (seek :pointer)
  (tell :pointer)
  (close :pointer))

(cffi:defcfun (head-parse "opus_head_parse") error
  (head :pointer)
  (data :pointer)
  (length :size))

(cffi:defcfun (granule-sample "opus_granule_sample") :int64
  (head :pointer)
  (granule-position :int64))

(cffi:defcfun (tags-parse "opus_tags_parse") error
  (tags :pointer)
  (data :pointer)
  (length :size))

(cffi:defcfun (tags-copy "opus_tags_copy") error
  (dst :pointer)
  (src :pointer))

(cffi:defcfun (tags-init "opus_tags_init") :void
  (tags :pointer))

(cffi:defcfun (tags-add "opus_tags_add") error
  (tags :pointer)
  (tag :string)
  (value :string))

(cffi:defcfun (tags-add-comment "opus_tags_add_comment") error
  (tags :pointer)
  (comment :string))

(cffi:defcfun (tags-set-binary-suffix "opus_tags_set_binary_suffix") error
  (tags :pointer)
  (data :pointer)
  (length :int))

(cffi:defcfun (tags-query "opus_tags_query") :string
  (tags :pointer)
  (tag :string)
  (count :int))

(cffi:defcfun (tags-query-count "opus_tags_query_count") :int
  (tags :pointer)
  (tag :string))

(cffi:defcfun (tags-get-binary-suffix "opus_tags_get_binary_suffix") :string
  (tags :pointer)
  (length :pointer))

(cffi:defcfun (tags-get-album-gain "opus_tags_get_album_gain") :int
  (tags :pointer)
  (gain-q8 :pointer))

(cffi:defcfun (tags-get-track-gain "opus_tags_get_track_gain") :int
  (tags :pointer)
  (gain-q8 :pointer))

(cffi:defcfun (tags-clear "opus_tags_clear") :void
  (tags :pointer))

(cffi:defcfun (tag-compare "opus_tagcompare") :int
  (tag-name :string)
  (comment :string))

(cffi:defcfun (tag-n-compare "opus_tagncompare") :int
  (tag-name :string)
  (tag-length :int)
  (comment :string))

(cffi:defcfun (picture-tag-parse "opus_picture_tag_parse") :int
  (picture :pointer)
  (tag :string))

(cffi:defcfun (picture-tag-init "opus_picture_tag_init") :void
  (picture :pointer))

(cffi:defcfun (picture-tag-clear "opus_picture_tag_clear") :void
  (picture :pointer))

(cffi:defcfun (server-info-init "opus_server_info_init") :void
  (server-info :pointer))

(cffi:defcfun (server-info-clear "opus_server_info_clear") :void
  (server-info :pointer))

(cffi:defcfun (fopen "op_fopen") :pointer
  (callbacks :pointer)
  (path :string)
  (mode :pointer))

(cffi:defcfun (fdopen "op_fdopen") :pointer
  (callbacks :pointer)
  (fd :int)
  (mode :pointer))

(cffi:defcfun (freopen "op_freopen") :pointer
  (callbacks :pointer)
  (path :string)
  (mode :pointer)
  (stream :pointer))

(cffi:defcfun (mem-stream-create "op_mem_stream_create") :pointer
  (callbacks :pointer)
  (data :pointer)
  (size :size))

(cffi:defcfun (test "op_test") error
  (head :pointer)
  (initial-data :pointer)
  (initial-bytes :size))

(cffi:defcfun (open-file "op_open_file") :pointer
  (path :string)
  (error :pointer))

(cffi:defcfun (open-memory "op_open_memory") :pointer
  (data :pointer)
  (size :size)
  (error :pointer))

(cffi:defcfun (open-callbacks "op_open_callbacks") :pointer
  (stream :pointer)
  (callbacks :pointer)
  (initial-data :pointer)
  (initial-bytes :size)
  (error :pointer))

(cffi:defcfun (test-file "op_test_file") :pointer
  (path :string)
  (error :pointer))

(cffi:defcfun (test-memory "op_test_memory") :pointer
  (data :pointer)
  (size :size)
  (error :pointer))

(cffi:defcfun (test-callbacks "op_test_callbacks") :pointer
  (stream :pointer)
  (callbacks :pointer)
  (initial-data :pointer)
  (initial-bytes :size)
  (error :pointer))

(cffi:defcfun (test-open "op_test_open") error
  (file :pointer))

(cffi:defcfun (free "op_free") :void
  (file :pointer))

(cffi:defcfun (seekable-p "op_seekable") :bool
  (file :pointer))

(cffi:defcfun (link-count "op_link_count") :int
  (file :pointer))

(cffi:defcfun (serial-number "op_serialno") :uint32
  (file :pointer)
  (link-index :int))

(cffi:defcfun (channel-count "op_channel_count") :int
  (file :pointer)
  (link-index :int))

(cffi:defcfun (raw-total "op_raw_total") :int64
  (file :pointer)
  (link-index :int))

(cffi:defcfun (pcm-total "op_pcm_total") :int64
  (file :pointer)
  (link-index :int))

(cffi:defcfun (head "op_head") :pointer
  (file :pointer)
  (link-index :int))

(cffi:defcfun (tags "op_tags") :pointer
  (file :pointer)
  (link-index :int))

(cffi:defcfun (current-link "op_current_link") :int
  (file :pointer))

(cffi:defcfun (bitrate "op_bitrate") :int32
  (file :pointer)
  (link-index :int))

(cffi:defcfun (bitrate-instant "op_bitrate_instant") :int32
  (file :pointer))

(cffi:defcfun (raw-tell "op_raw_tell") :int64
  (file :pointer))

(cffi:defcfun (pcm-tell "op_pcm_tell") :int64
  (file :pointer))

(cffi:defcfun (raw-seek "op_raw_seek") error
  (file :pointer)
  (byte-offset :int64))

(cffi:defcfun (pcm-seek "op_pcm_seek") error
  (file :pointer)
  (pcm-offset :int64))

(cffi:defcfun (set-decode-callback "op_set_decode_callback") :void
  (file :pointer)
  (function :pointer)
  (user :pointer))

(cffi:defcfun (set-gain-offset "op_set_gain_offset") error
  (file :pointer)
  (gain-type gain-type)
  (offset-q8 :int32))

(cffi:defcfun (set-dither-enabled "op_set_dither_enabled") error
  (file :pointer)
  (enabled :bool))

(cffi:defcfun (read "op_read") error
  (file :pointer)
  (pcm :pointer)
  (buffer-size :int)
  (link-index :pointer))

(cffi:defcfun (read-float "op_read_float") error
  (file :pointer)
  (pcm :pointer)
  (buffer-size :int)
  (link-index :pointer))

(cffi:defcfun (read-stereo "op_read_stereo") error
  (file :pointer)
  (pcm :pointer)
  (buffer-size :int))

(cffi:defcfun (read-stereo-float "op_read_stereo_float") error
  (file :pointer)
  (pcm :pointer)
  (buffer-size :int))
