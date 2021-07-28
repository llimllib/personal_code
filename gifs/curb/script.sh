
# to download only part of a video:
# ffmpeg $(youtube-dl -g 'https://www.youtube.com/watch?v=pYBEOtemA40' | sed "s/.*/-ss 00:22 -i &/") -t 00:11 -c copy out.mkv

# https://ffmpeg.org/ffmpeg-filters.html#drawtext-1
ffmpeg -y -ss 0.0 -t 10.5 -i "out.mkv" -filter_complex_script filter.ffm "curb.gif"
