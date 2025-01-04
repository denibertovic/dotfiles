conky.config = {
	alignment = "top_right",
	background = false,
	color1 = "A9A9A9",
	color2 = "616161",
	color3 = "313131",
	cpu_avg_samples = 4,
	default_color = "FFFFFF",
	default_outline_color = "black",
	default_shade_color = "333333",
	double_buffer = true,
	draw_borders = false,
	draw_graph_borders = false,
	draw_outline = false,
	draw_shades = false,
	font = "Ubuntu:style=medium:pixelsize=18",
	gap_x = 20,
	gap_y = 0,
	-- lua_load = "/home/deni/.config/conky/conky.lua",
	lua_draw_hook_post = "conky_main",
	maximum_width = 300,
	minimum_height = 10,
	minimum_width = 300,
	net_avg_samples = 2,
	no_buffers = true,
	override_utf8_locale = true,
	own_window_argb_value = 0,
	own_window_argb_visual = true,
	own_window_colour = "000000",
	own_window_hints = "undecorated,below,sticky,skip_taskbar,skip_pager",
	own_window_transparent = true,
	own_window_type = "desktop",
	own_window = true,
	text_buffer_size = 2048,
	total_run_times = 0,
	update_interval = 3,
	uppercase = false,
	use_xft = true,
	xftalpha = 0.1,
}

conky.text = [[

${color1}

${color1}${font Font Awesome 5 Free:style=Solid:pixelsize=14}${font}${goto 30}SYSTEM

$sysname $kernel $machine

${font Font Awesome 5 Free:style=Solid:pizelsize=14}${font}${goto 30}${if_match "${hwmon 3 temp 1}">="90"}${color red}${hwmon 3 temp 1}°${color}${else}${hwmon 3 temp 1}°${endif}${goto 100}${font Font Awesome 5 Free:style=Solid:pizelsize=14} ${font} ${hwmon 3 fan 1} rpm${alignr}${font Font Awesome 5 Free:style=Solid:pizelsize=14} ${font} ${uptime}

${color3}${hr}

${color1}${font Font Awesome 5 Free:style=Solid:pixelsize=14}${font}${goto 30}STORAGE${alignr}${fs_free} free / ${fs_size}
${color2}${diskiograph_read 50,145}${alignr}${diskiograph_write 50,145}
${color3}${hr}

${color1}${font Font Awesome 5 Free:style=Solid:pixelsize=14}${font}${goto 30}NETWORK

${font Font Awesome 5 Free:style=Solid:pixelsize=14}${font}${goto 30}${if_match "${addr enp0s31f6}"!="No Address"}${addr enp0s31f6}${endif}${if_match "${addr wlp0s20f3}"!="No Address"}${addr wlp0s20f3}${endif}${alignr}${font Font Awesome 5 Free:style=Solid:pixelsize=14} ${font} ${execi 3600 curl ipinfo.io/ip}
${if_match "${addr enp0s31f6}"!="No Address"}${color1}${font Font Awesome 5 Free:style=Solid:pixelsize=14}${font}${goto 30}${downspeedf enp0s31f6}k/s (${totaldown enp0s31f6})${alignr}${font Font Awesome 5 Free:style=Solid:pixelsize=14} ${font} ${upspeedf enp0s31f6}k/s (${totalup enp0s31f6})${endif}
${if_match "${addr wlp0s20f3}"!="No Address"}${color1}${font Font Awesome 5 Free:style=Solid:pixelsize=14}${font}${goto 30}${downspeedf wlp0s20f3}k/s (${totaldown wlp0s20f3})${alignr}${font Font Awesome 5 Free:style=Solid:pixelsize=14} ${font} ${upspeedf wlp0s20f3}k/s (${totalup wlp0s20f3})${endif}

${color3}${hr}

${color1}${font Font Awesome 5 Free:style=Solid:pixelsize=14}${font}${goto 30} BACKUP

zrepl status: ${alignr}${execpi 10 /home/deni/scripts/zrepl_backup_status.sh}



]]
