# -*- MakeFile -*-
all:
	stow --verbose --target=$$HOME --restow */

hyprpapers:
	$(eval resolution := "3440x1440")
	declare -A PAPERS=( \
		[1]="#93A1A1-#586E75" \
		[2]="#DDF28A-#4E5A04" \
		[3]="#D7E7FF-#06605A" \
		[4]="#9AF9EF-#06605A" \
		[5]="#DFE1FE-#3D4181" \
		[6]="#FFE4BD-#6B5001" \
		[7]="#FEDAD5-#7C2B0C" \
		[8]="#FFD9D9-#871B19" \
		[9]="#FFD8E5-#831A4E" \
	); for key in "$${!PAPERS[@]}" ; do \
		/usr/bin/magick -size $(resolution) -define gradient:direction=southeast gradient:"$${PAPERS[$${key}]}" wayland/.config/hypr/workspace-$${key}.png ; \
	done

delete:
	stow --verbose --target=$$HOME --delete */
