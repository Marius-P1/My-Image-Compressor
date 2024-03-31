
##
## EPITECH PROJECT, 2024
## My-Image-Compressor
## File description:
## 3Dgraph
##

#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import proj3d
import sys
# Parse the file
filename = sys.argv[1]
with open(filename, 'r') as file:
    lines = file.readlines()


# Initialize empty lists for r, g, b color
r = []
g = []
b = []
# Initialize empty list for clusters
cluster_r = []
cluster_g = []
cluster_b = []



for line in lines:
    words = line.split()
    num_words = len(words)
    if num_words != 2:
        if line.startswith('-'):
            continue
        cluster = words[0].strip('()').split(',')
        cluster_r.append(int(cluster[0]))
        cluster_g.append(int(cluster[1]))
        cluster_b.append(int(cluster[2]))
        print(cluster)
        continue
    # Split the second word into coordinates
    color = words[1].strip('()').split(',')
    # Append the coordinates to the respective lists
    r.append(int(color[0]))
    g.append(int(color[1]))
    b.append(int(color[2]))


fig = plt.figure(figsize=(8, 8))
ax = fig.add_subplot(111, projection='3d')

# Convert the RGB values to the [0, 1] scale
colors = [[ri/255, gi/255, bi/255] for ri, gi, bi in zip(r, g, b)]

# Add label to the cluster
for i in range(len(cluster_r)):
    cluster_colors = [cluster_r[i]/255, cluster_g[i]/255, cluster_b[i]/255]
    ax.scatter(cluster_r[i], cluster_g[i], cluster_b[i], s=100, label=f'Cluster {i}', c=cluster_colors)
ax.legend()

# To improve performance and avoid displaying all points in the same zone, we can sample a subset of the points.
sample_rate = 0.1  # Adjust this value to change the number of points displayed. 0.1 means 10% of points will be displayed.
sample_indices = np.random.choice(len(r), size=int(len(r)*sample_rate), replace=False)

sampled_r = [r[i] for i in sample_indices]
sampled_g = [g[i] for i in sample_indices]
sampled_b = [b[i] for i in sample_indices]
sampled_colors = [colors[i] for i in sample_indices]

ax.scatter(sampled_r, sampled_g, sampled_b, c=sampled_colors)
plt.show()
