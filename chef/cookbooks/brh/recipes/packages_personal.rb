# This recipe should only be installed on machines that
# I will be using for substantial periods of time.
include_recipe 'apt'

package 'awesome'
package 'icedtea-netx'
package 'icedtea-plugin'
package 'keepass2'

# Browsers
package 'chromium-browser'
package 'flashplugin-installer'

# Media
package 'calibre'
package 'gimp'
package 'imagemagick'
package 'libav-tools'
package 'libavcodec-extra'
package 'mupdf'
package 'vlc'

# Networking
package 'nmap'
package 'vinagre'

# Other
package 'htop'
package 'weather-util'

