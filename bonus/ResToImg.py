
##
## EPITECH PROJECT, 2024
## My-Image-Compressor
## File description:
## retoimg
##

#!/usr/bin/env python3
import sys
from PIL import Image
# Parse the file
filename = sys.argv[1]
with open(filename, 'r') as file:
    lines = file.readlines()


# Initialize empty lists for r, g, b color
pos_x = [[] for _ in range(1000)]
pos_y = [[] for _ in range(1000)]
# Initialize empty list for clusters
cluster_r = []
cluster_g = []
cluster_b = []

iter_pos = -1



for line in lines:
    words = line.split()
    num_words = len(words)
    if num_words != 2:
        if line.startswith('-'):
            if line.startswith('--'):
                iter_pos += 1
            continue
        cluster = words[0].strip('()').split(',')
        cluster_r.append(int(cluster[0]))
        cluster_g.append(int(cluster[1]))
        cluster_b.append(int(cluster[2]))
        print(cluster)
        continue
    # Split the second word into coordinates
    pos = words[0].strip('()').split(',')
    # Append the coordinates to the respective lists
    pos_x[iter_pos].append(int(pos[0]))
    pos_y[iter_pos].append(int(pos[1]))


# Create a xmp image based on the size of the image
# Determine the size of the image
max_x = max([max(x) for x in pos_x if x])
max_y = max([max(y) for y in pos_y if y])

# Create a new image with the determined size
img = Image.new('RGB', (max_x + 1, max_y + 1))

# Populate the image with the cluster colors at the respective positions
for i in range(iter_pos + 1):
    for j in range(len(pos_x[i])):
        img.putpixel((pos_x[i][j], pos_y[i][j]), (cluster_r[i], cluster_g[i], cluster_b[i]))

# Save the image
img.save('output_image.bmp')
