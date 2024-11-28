# -*- MakeFile -*-
all:
	stow --verbose --target=$$HOME --restow */

hyprpapers:
	# NOTE that we need the pastel command/package to generate these: https://github.com/sharkdp/pastel . It's in the AUR
	$(eval resolution := "3440x1440")
	$(eval dest := "wayland/.config/hypr/workspace-")
	$(eval ext := ".png")
	$(eval MAGICK := "/usr/bin/magick" "-define" "png:exclude-chunks=date") # reproducable-build magick png's
	# I mostly used https://colorkit.co/palette/2c4875-5b4c82-8a508f-cc0863-ff6361-ff8531-ffa600-8cb357-18bfae-53cbef/ for this color scheme.
	declare -A PAPERS=( \
		[1]="#586e75" \
		[2]="#2c4875" \
		[3]="#5b4c82" \
		[4]="#8a508f" \
		[5]="#ff6361" \
		[6]="#ff8531" \
		[7]="#ffa600" \
		[8]="#8cb357" \
		[9]="#79021c" \
	); for key in "$${!PAPERS[@]}" ; do \
    darkColor=$${PAPERS[$${key}]}; \
    lightColor=$$(/usr/bin/pastel lighten 0.20 $${darkColor} | /usr/bin/pastel format hex); \
    darkerColor=$$(/usr/bin/pastel darken 0.20 $${darkColor} | /usr/bin/pastel format hex); \
		$(MAGICK) -size $(resolution) -define gradient:direction=southeast gradient:"$${darkColor}-$${lightColor}" $(dest)$${key}$(ext) ; \
		$(MAGICK) -size $(resolution) -define gradient:direction=southeast gradient:"$${darkerColor}-$${darkColor}" $(dest)$${key}special$(ext) ; \
	done; \
	$(MAGICK) xc:$${PAPERS[1]} xc:$${PAPERS[2]} xc:$${PAPERS[3]} xc:$${PAPERS[4]} xc:$${PAPERS[5]} xc:$${PAPERS[6]} xc:$${PAPERS[7]} xc:$${PAPERS[8]} xc:$${PAPERS[9]} \
          +append -filter Cubic -resize $(resolution)\! $(dest)expo$(ext)

delete:
	stow --verbose --target=$$HOME --delete */
