
################################################################################

## Prerequisite code

################################################################################
library(scico)
library(devtools)
library(sf)
library(raster)
library(tidyverse)
library(purrr)
source("./helpers/sf_to_trimesh.R")

### Make a tight bounding box.
### coords come from a google map: https://drive.google.com/open?id=1Ak26Hyx1R-f2QjPCTK0rQLye5xcHyE8n&usp=sharing

uluru_bbox <-
  st_bbox(c(xmin = 131.02084,
            xmax = 131.0535,
            ymin = -25.35461,
            ymax = -25.33568),
          crs = st_crs("+proj=longlat +ellps=WGS84"))

### Convert to a MULTIPOLYGON
uluru_bbox_mpoly <-
  uluru_bbox %>%
    st_as_sfc() %>%
    st_multipolygon() %>%
    st_geometry()

st_crs(uluru_bbox_mpoly) <- st_crs(uluru_bbox)

### Read in raster
nt_raster <- raster("./data/ELVIS_CLIP.tif")

### Homogenise bbox and raster CRS
uluru_bbox_mpoly <-
  st_transform(uluru_bbox_mpoly, crs = crs(nt_raster)@projargs)

### Plot cropped raster for sanity check
nt_raster %>%
  crop(st_bbox(uluru_bbox_mpoly)[c("xmin","xmax","ymin","ymax")]) %>%
  plot() ## looks good!

### Triangulate bbox
uluru_bbox_trimesh <-
  sf_to_trimesh(uluru_bbox_mpoly, 12000) # a few more than last example for finer mesh.

### Add elevation to trimesh
ul_extent_elev <-
  raster::extract(nt_raster, uluru_bbox_trimesh$P[,1:2])
uluru_bbox_trimesh$P <-
  cbind(uluru_bbox_trimesh$P, ul_extent_elev)


################################################################################

## Uluru Mesh to VR

################################################################################

## install r2vr using devtools
install_github('milesmcbain/r2vr')
library(r2vr)

## load JSON conversion helper function
source("./helpers/trimesh_to_threejson.R")

## load colour palette index helper function.
source("./helpers/colour_from_scale.R")

## After prerequisite code, our mesh is now in uluru_bbox_trimesh.

## Task 1: Create a colour for each vertex from height raster

## Our vertex heights are in
head(uluru_bbox_trimesh$P[,3])

## range of vertex heights
z_range <- range(uluru_bbox_trimesh$P[,3])

# So we use a palette function to transform to colours
n_colours <- 256
palette_function <- purrr::partial(scico, palette = "tokyo")

colour_data <- colour_from_scale(uluru_bbox_trimesh$P[,3], palette_function, n_colours)


## Task 2: Generate vertex normals

## Taks 3: Use a texture

## write to JSON
mesh_json <- trimesh_to_threejson(vertices = uluru_bbox_trimesh$P, face_vertices = uluru_bbox_trimesh$T)
write_lines(mesh_json, "./data/uluru_mesh.json")
