(in-package #:org.shirakumo.fraf.opus)

(docs:define-docs
  (type opus-error
    "Condition signalled on failed API calls.

See FILE
See CODE")
  
  (function file
    "Returns the FILE instance associated with the error, if any.

See FILE (type)
See OPUS-ERROR")
  
  (function code
    "Returns the error code returned by the failed API call.

See OPUS-ERROR")
  
  (function init
    "Initialises the Opus API.

See SHUTDOWN")
  
  (function shutdown
    "Closes the Opus API.

See INIT")
  
  (function check-return
    "Checks whether the return value was valid or an error.

If valid, returns the value. Otherwise, signals an OPUS-ERROR.

See OPUS-ERROR (type)")
  
  (type file
    "Representation of an Opus file.

Use one of the OPEN* functions to obtain this instance. when you are
done, use CLOSE to properly dispose of the file.

See HANDLE
See READ-FUN
See SEEK-FUN
See INDEX-FUN
See CLOSE-FUN
See CLOSE
See OPEN-FILE
See OPEN-POINTER
See OPEN-VECTOR
See OPEN-CALLBACK
See OPEN
See WITH-FILE
See SAMPLERATE
See CHANNELS
See SAMPLE-COUNT
See DURATION
See SEEKABLE-P
See LINK-COUNT
See SERIAL-NUMBER
See CURRENT-LINK
See BITRATE
See INSTANT-BITRATE
See INDEX
See SEEK
See COMMENTS
See VENDOR
See GAIN
See DECODE-FLOAT
See DECODE-INT16")
  
  (function handle
    "Returns the internal foreign pointer handle of the file.

See FILE (type)")
  
  (function read-fun
    "Accesses the read function for callback-driven files.

The function must accept three arguments:
  FILE    --- The FILE instance.
  BUFFER  --- A pointer to a buffer to fill.
  BYTES   --- The number of bytes in the buffer to fill.

It must return the number of bytes written to the buffer or -1 on
error.

See FILE (type)")
  
  (function seek-fun
    "Accesses the seek function for callback-driven files.

The function must accept three arguments:
  FILE    --- The FILE instance.
  OFFSET  --- The offset in bytes.
  WHENCE  --- One of:
    :START   --- OFFSET is from the start of the file.
    :CURRENT --- OFFSET is from the current index.
    :END     --- OFFSET is from the end of the file.

It should return 0 on success or -1 on error.

See FILE (type)")
  
  (function index-fun
    "Accesses the index function for callback-driven files.

The function must accept one argument:
  FILE    --- The FILE instance.

It should return the byte index of the stream, or -1 on error.

See FILE (type)")
  
  (function close-fun
    "Accesses the close function for callback-driven files.

The function must accept one argument:
  FILE    --- The FILE instance.

It should return 0 on success or -1 on error.

See FILE (type)")
  
  (function close
    "Close the file and free its resources.

Once closed, a file cannot be reopened and you must create a new file
instead.

See FILE (type)")
  
  (function open-file
    "Open a file from a filesystem PATH.

Automatically calls INIT.

See INIT
See FILE (type)")
  
  (function open-pointer
    "Open a file from a raw pointer.

You may specify START to offset from the pointer, and you must pass
END to designate the buffer's maximum length.

Automatically calls INIT.

See INIT
See FILE (type)")
  
  (function open-vector
    "Open a file from a static vector.

You may specify START and END to define a sub region within the
vector. The vector MUST have been created with
STATIC-VECTORS:MAKE-STATIC-VECTOR.

Automatically calls INIT.

See INIT
See FILE (type)")
  
  (function open-callback
    "Open a file using the callbacks API.

While the seek, index, and close functions are optional, it is highly
recommended that you pass them as well as the read function. You may
pass INITIAL-DATA (a static vector or foreign pointer) if you've
already begun processing part of the data stream.

Automatically calls INIT.

See INIT
See READ-FUN
See SEEK-FUN
See INDEX-FUN
See CLOSE-FUN
See FILE (type)")

  (function open-stream
    "Open a file from a stream.

Note that seeking is only supported if the stream is a FILE-STREAM.
This uses the callbacks API in the back.

Automatically calls INIT.

See INIT
See OPEN-CALLBACK
See FILE (type)")
  
  (function open
    "Open a file from the given storage.

The storage may either be a STRING, PATHNAME, CFFI:FOREIGN-POINTER,
STATIC-VECTOR, FUNCTION, or STREAM.

See OPEN-FILE
See OPEN-POINTER
See OPEN-VECTOR
See OPEN-CALLBACK
See OPEN-STREAM
See FILE (type)")
  
  (function with-file
    "Open a file within a dynamic extent.

Automatically calls OPEN, and CLOSE on unwind.

See OPEN
see CLOSE
See FILE (type)")
  
  (function samplerate
    "Returns the samplerate of the file.

This is fixed to 48000 Hz.

See FILE (type)")

  (function channels
    "Returns the number of channels in the file.

Returns the number of channels in the file or link.
The channel layout is as follows:

  1 --- center
  2 --- left, right
  3 --- left, center, right
  4 --- front left, front right, rear left, rear right
  5 --- front left, center, front right, rear left, rear right
  6 --- front left, center, front right, rear left, rear right, LFE
  7 --- front left, center, front right, side left, side right, rear
        center, LFE
  8 --- front left, center, front right, side left, side right, rear
        left, rear right, LFE

For any other number of channels the layout is up to the application.

See FILE (type)")
  
  (function sample-count
    "Returns the total number of samples (frames!) in the file or link.

See FILE (type)")
  
  (function duration
    "Returns the total duration in seconds of the file or link.

See FILE (type)")
  
  (function seekable-p
    "Returns whether the file is seekable or not.

See FILE (type)")
  
  (function link-count
    "Returns the total number of links in the file.

See FILE (type)")
  
  (function serial-number
    "Returns the vendor serial number of the (current) link.

See FILE (type)")
  
  (function current-link
    "Returns the current link index.

See FILE (type)")
  
  (function bitrate
    "Returns the bitrate of the (current) link.

See FILE (type)")
  
  (function instant-bitrate
    "Returns the bitrate between now and the last time this function was called.

See FILE (type)")
  
  (function index
    "Accesses the current sample (frame!) index.

See FILE (type)")
  
  (function seek
    "Seeks the file to the specified place.

  BY   --- May be one of:
    :SECOND   --- The index is a float in seconds
    :SAMPLE   --- The index is an integer in samples (frames!)
    :BYTE     --- The index is an integer in bytes of the stream
  MODE --- May be one of:
    :ABSOLUTE --- The index is from the beginning of the file
    :RELATIVE --- The index is relative to the current position

See (SETF INDEX)
See FILE (type)")
  
  (function comments
    "Returns a list of user comments contained in the file or link.

See FILE (type)")
  
  (function vendor
    "Returns the vendor identification string of the file or link.

See FILE (type)")
  
  (function gain
    "Accesses the gain in dB of the file.

See FILE (type)")
  
  (function decode-float
    "Decodes the file into the float buffer.

You may designate the region of the buffer to fill by START and
END. Returns the index after the last element in the buffer that was
set, same as READ-SEQUENCE.

The buffer must be a (SIMPLE-ARRAY SINGLE-FLOAT (*)), where the
data is represented as frames of interleaved channels.

If STEREO is T, remixes the signal to stereo regardless of the actual
channel count.

See FILE (type)")
  
  (function decode-int16
    "Decodes the file into the (unsigned-byte 16) buffer.

You may designate the region of the buffer to fill by START and
END. Returns the index after the last element in the buffer that was
set, same as READ-SEQUENCE.

The buffer must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*)), where the
data is represented as frames of interleaved channels.

If STEREO is T, remixes the signal to stereo regardless of the actual
channel count.

See FILE (type)"))
