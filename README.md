# elm-mpv-webui
...is a web based user interface with controls for the [mpv mediaplayer](https://mpv.io/).

Written in elm, elm-ui and lua.

## Usage

### Elm

Install elm https://guide.elm-lang.org/install/elm.html then run:

```bash
$ git clone https://github.com/rofrol/elm-mpv-webui
$ elm make webui-page/src/Main.elm --output webui-page/webui.js
```

### Lua

Tested on Ubuntu 19.10.

```bash
$ sudo apt install lua5.2 lua5.2-dev luarocks
$ sudo luarocks install luasocket
```

### Install in mpv

```bash
$ mkdir -p ~/.config/mpv/scripts
$ cd ~/.config/mpv/scripts
$ ln -s elm-mpv-webui/webui.lua .
$ ln -s elm-mpv-webui/webui-page/ .
```

Alternatively you can also use the `--script` option from mpv or add something like
`scripts-add=/path/to/elm-mpv-webui/webui.lua` to `mpv.conf`.

To find all videos files in directory and start playing them:

`find "$1" -type f -exec file -N -i -- {} + | sed -n 's!: video/[^:]*$!!p' | rg -v ".*\.sub$" | awk '{printf "\"%s\"\n", $0}' | xargs mpv`

### Web browser

You can access the webui when accessing [http://127.0.0.1:8080](http://127.0.0.1:8080) or
[http://[::1]:8080](http://[::1]:8080) in your webbrowser.

By default it listens on `0.0.0.0:8080` and `[::0]:8080`. As described below, this can be changed.

### Options
Options can be set with [--script-opts](https://mpv.io/manual/master/#options-script-opts)
with the prefix `webui-`.

#### port (int)
Set the port to serve the webui (default: 8080). Setting this allows for
running multiple instances on different ports.

Example:

```
webui-port=8000
```

#### ipv4 (bool)
Enable listening on ipv4 (default: yes)

Example:

```
webui-ipv4=no
```

#### ipv6 (bool)
Enable listening on ipv6 (default: yes)

Example:

```
webui-ipv6=no
```

#### disable (bool)
Disable webui (default: no)

Example:

```
webui-disable=yes
```

#### logging (bool)
Log requests to STDOUT (default: no)

Example:

```
webui-logging=yes
```

#### audio-devices (string)
Set the audio-devices used for cycling. By default it uses all interfaces MPV
knows of.

You can see a list of them with following command:

```shell
mpv --audio-device=help
```

Example:

```
webui-audio-devices="pulse/alsa_output.pci-0000_00_1b.0.analog-stereo pulse/alsa_output.pci-0000_00_03.0.hdmi-stereo"
```

### Authentication
There is a very simple implementation of
[Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication). It will be enabled, if a file
`.htpasswd` exists in the same directory as `webui.lua`. The file needs to
contain data in the following format:

```
user1:password1
user2:password2
```
Only plaintext `.htpasswd` entries are supported.

## Screenshots
![webui screenshot](screenshots/webui.png#2)

![playlist screenshot](screenshots/playlist.png#1)

## Endpoints
You can also directly talk to the endpoints:

| URI                        | Method | Parameter                          | Description                                                             |
| -------------------------- | ------ | ---------------------------------- | ----------------------------------------------------------------------- |
| /api/status                | GET    |                                    | Returns JSON data about playing media --> see below                     |
| /api/play                  | POST   |                                    | Play media                                                              |
| /api/pause                 | POST   |                                    | Pause media                                                             |
| /api/toggle_pause          | POST   |                                    | Toggle play/pause                                                       |
| /api/fullscreen            | POST   |                                    | Toggle fullscreen                                                       |
| /api/seek/:seconds         | POST   | `int` or `float` (can be negative) | Seek                                                                    |
| /api/set_position/:seconds | POST   |                                    | Go to position :seconds                                                 |
| /api/playlist_prev         | POST   |                                    | Go to previous media in playlist                                        |
| /api/playlist_next         | POST   |                                    | Go to next media in playlist                                            |
| /api/playlist_jump/:pos    | POST   | `int`                              | Jump to playlist item at position `:pos`                                |
| /api/add_chapter/:amount   | POST   | `int` (can be negative)            | Jump `:amount` chapters in current media                                |
| /api/add_volume/:percent   | POST   | `int` or `float` (can be negative) | Add :percent% volume                                                    |
| /api/set_volume/:percent   | POST   | `int` or `float`                   | Set volume to :percent%                                                 |
| /api/add_sub_delay/:ms     | POST   | `int` or `float` (can be negative) | Add :ms milliseconds subtitles delay                                    |
| /api/set_sub_delay/:ms     | POST   | `int` or `float` (can be negative) | Set subtitles delay to :ms milliseconds                                 |
| /api/add_audio_delay/:ms   | POST   | `int` or `float` (can be negative) | Add :ms miliseconds audio delay                                         |
| /api/set_audio_delay/:ms   | POST   | `int` or `float` (can be negative) | Set audio delay to :ms milliseconds                                     |
| /api/cycle_sub             | POST   |                                    | Cycle trough available subtitles                                        |
| /api/cycle_audio           | POST   |                                    | Cycle trough available audio tracks                                     |
| /api/cycle_audio_device    | POST   |                                    | Cycle trough audio devices. [More information.](#audio-devices-string)  |

All POST endpoints return a JSON message. If successful: `{"message": "success"}`, otherwise, the message will contain
information about the error.

### /api/status
`metadata` contains all the metadata mpv can see, below is just an example:

``` json
{
    "filename": "big_buck_bunny_1080p_stereo.ogg",
    "duration": 596,      # <-- seconds
    "position": 122,      # <-- seconds
    "pause": true,
    "remaining": 474,     # <-- seconds
    "sub-delay": 0,       # <-- milliseconds
    "audio-delay": 0,     # <-- milliseconds
    "fullscreen": false,
    "metadata": {},       # <-- All metadata available to MPV
    "track-list": [       # <-- All available video, audio and sub tracks
        {
            "id": 1,
            "type": "video",
            "src-id": 0,
            "albumart": false,
            "default": false,
            "forced": false,
            "external": false,
            "selected": true,
            "ff-index": 0,
            "decoder-desc": "theora (Theora)",
            "codec": "theora",
            "demux-w": 1920,
            "demux-h": 1080
        },
        {
            "id": 1,
            "type": "audio",
            "src-id": 0,
            "audio-channels": 2,
            "albumart": false,
            "default": false,
            "forced": false,
            "external": false,
            "selected": true,
            "ff-index": 1,
            "decoder-desc": "vorbis (Vorbis)",
            "codec": "vorbis",
            "demux-channel-count": 2,
            "demux-channels": "stereo",
            "demux-samplerate": 48000
        }
    ],
    "volume": 64,
    "volume-max": 130,
    "playlist": [         # <-- All files in the current playlist
        {
            "filename": "Videos/big_buck_bunny_1080p_stereo.ogg",
            "current": true,
            "playing": true
        }
    ]
}
```

## Thanks
Thanks to [open-dynaMIX](https://github.com/open-dynaMIX) and [makedin](https://github.com/makedin) for their work on this.

## Differences to mpv-web-ui
 - More media controls
 - Playlist controls
 - Some styles and font-awesome
 - ipv6 support
 - Option to set the port being used (defaults to 8080)
 - Using the Media Session API

## Warning
These are my first steps with lua, so I'm just happy it works.
