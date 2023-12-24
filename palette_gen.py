import math
import colorsys

hue_num = 15
lum_num = 16

pal_file = open("palette.inc", "wt")

print("pal_sz equ 256", file = pal_file)
print("", file = pal_file)
print("palette:", file = pal_file)
for lum in range(lum_num):
    rgb_col = colorsys.hls_to_rgb(0.0, lum / (lum_num - 1), 0.0)
    rgb_col_int = []
    for comp in rgb_col:
        rgb_col_int.append(int(math.floor(comp * 0x3f + 0.5)))
    print("    db {0:#x}, {1:#x}, {2:#x}".format(*rgb_col_int), file = pal_file)
for hue in range(hue_num):
    for lum in range(lum_num):
        rgb_col = colorsys.hls_to_rgb(hue / hue_num, lum / (lum_num - 1), 1.0)
        rgb_col_int = []
        for comp in rgb_col:
            rgb_col_int.append(int(math.floor(comp * 0x3f + 0.5)))
        print("    db {0:#x}, {1:#x}, {2:#x}".format(*rgb_col_int), file = pal_file)
