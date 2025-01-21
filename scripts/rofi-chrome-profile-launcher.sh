
#!/usr/bin/env bash

# List of supported Chromium-based browsers and their configuration directories
BROWSERS=(
    "google-chrome-stable:$HOME/.config/google-chrome"
    "chromium:$HOME/.config/chromium"
    "brave:$HOME/.config/BraveSoftware/Brave-Browser"
)

declare -A browser_profiles=()

# Function to fetch profiles for a given browser
get_profiles() {
    local browser=$1
    local config_dir=$2

    # Check if the browser's user data directory exists
    if [ -d "$config_dir" ]; then
        # Attempt to read the profiles from the Local State file
        if [ -f "$config_dir/Local State" ]; then
            DATA=$(python3 << END
import json
try:
    with open("$config_dir/Local State") as f:
        data = json.load(f)
    for profile in data["profile"]["info_cache"]:
        print("%s_____%s" % (profile, data["profile"]["info_cache"][profile]["name"]))
except Exception as e:
    pass
END
            )

            # Populate browser_profiles array
            while read -r line; do
                PROFILE="${line%_____*}"
                NAME="${line#*_____}"
                browser_profiles["$browser|$NAME"]="$PROFILE"
            done <<< "$DATA"
        fi
    fi
}

# Gather profiles for all supported browsers
for entry in "${BROWSERS[@]}"; do
    IFS=":" read -r browser config_dir <<< "$entry"
    get_profiles "$browser" "$config_dir"
done

if [ -z "$@" ]; then
    # No argument passed, meaning that rofi was launched: show the profiles
    profiles_sorted=( $(for key in "${!browser_profiles[@]}"; do echo "$key"; done | sort) )
    counter=1
    for key in "${profiles_sorted[@]}"; do
        IFS="|" read -r browser profile <<< "$key"
        echo "$counter. $browser: $profile"
        ((counter++))
    done
else
    # One argument passed, meaning that the user selected a profile: launch the browser
    SELECTION="$@"

    # Extract the browser and profile from the selection
    SELECTION="${SELECTION#*. }" # Remove the numbering prefix
    selected_browser="${SELECTION%%:*}" # Extract the browser
    selected_profile="${SELECTION#*: }" # Extract the profile

    # Reconstruct the key and fetch the profile directory
    selected_key="$selected_browser|$selected_profile"
    profile_dir="${browser_profiles["$selected_key"]}"

    if [ -n "$profile_dir" ]; then
        "$selected_browser" --profile-directory="$profile_dir" --force-device-scale-factor=1.0 > /dev/null 2>&1 &
    else
        echo "Invalid selection: $SELECTION"
        exit 1
    fi
fi
