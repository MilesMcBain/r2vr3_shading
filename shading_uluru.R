
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

### Crop raster and sanity check
nt_raster_cropped <-
  nt_raster %>%
  crop(st_bbox(uluru_bbox_mpoly)[c("xmin","xmax","ymin","ymax")])

## plot(nt_raster_cropped) ## looks good!

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
##install_github('milesmcbain/r2vr')

library(r2vr)

## load JSON conversion helper function
source("./helpers/trimesh_to_threejson.R")

## load colour palette index helper function.
source("./helpers/colour_from_scale.R")

## load vertex to face colour conversion
source("./helpers/vertex_to_face_colours.R")

## After prerequisite code, our mesh is now in uluru_bbox_trimesh.

## Task 1: Create a colour for each vertex from height raster

## Our vertex heights are in
## head(uluru_bbox_trimesh$P[,3])

## So we use a palette function to transform to colours
n_colours <- 256
palette_function <- purrr::partial(scico, palette = "tokyo")

vertex_colour_data <- colour_from_scale(uluru_bbox_trimesh$P[,3], palette_function,
                                 n_colours, zero_index = TRUE)

face_colours <- vertex_to_face_colours(vertex_colour_data$indexes, uluru_bbox_trimesh$T)


## Generate a shaded model JSON
mesh_json <- trimesh_to_threejson(vertices = uluru_bbox_trimesh$P, face_vertices = uluru_bbox_trimesh$T, colours = vertex_colour_data$colours, face_vertex_colours = face_colours)


## We need to correct the height based on ground height.
## In this case we'll find ground height from the  highest corner of the bounding box.
ground_height <- 
 max(raster::extract(nt_raster, uluru_bbox_mpoly[[1]][[1]][[1]], nrow = 1))

height_correction <- -1 * (ground_height - mean(uluru_bbox_trimesh$P[,3]))
## We're reversing the correction that would have been applied to the
## ground height by centering.

## Rotated and height corrected render:

scale_factor <- 0.01

uluru_json <-
  a_in_mem_asset(data = mesh_json,
                 id = "uluru",
                 src = "./uluru_mesh.json")

uluru <-
  a_json_model(src_asset = uluru_json,
               id = "rock",
               scale = scale_factor*c(1,1,1),
               position = c(0,0 + height_correction * scale_factor,-15),
               rotation = c(-90, 180, 0)
               )

sky <- a_entity(tag = "sky",
                color = "#000000")

aframe_scene2 <-
  a_scene(template = "empty",
          title = "Uluru Mesh",
          description = "An A-Frame scene of Uluru",
          children = list(uluru, sky))

aframe_scene2$serve()
browseURL("http://127.0.0.1:8080")

## don't forget to:
aframe_scene2$stop()

## Task 2: Generate vertex normals

## Option 1 turn on normals in threejs using
## https://github.com/donmccurdy/aframe-extras

uluru <-
  a_json_model(src_asset = uluru_json,
               mesh_smooth = TRUE,
               scale = scale_factor*c(1,1,1),
               position = c(0,0 + height_correction * scale_factor, -15),
               rotation = c(-90, 180, 0))

aframe_scene2 <-
  a_scene(template = "empty",
          title = "Uluru Mesh",
          description = "An A-Frame scene of Uluru",
          children = list(uluru, sky))

aframe_scene2$serve()

## don't forget to:
aframe_scene2$stop()


## Taks 3: Use a texture
library(dismo)

## there are better ways to get imagery, but this is a good old faithful detault
im <- dismo::gmap(nt_raster_cropped, type = "satellite", scale = 2)

## so this is a bit head-stretching, because multiple coordinate systems in play

## we want the [0,1,0,1] coordinates of the image in terms of the geographic mesh
## 1) mesh is lcc, backwards to longlat WGS84
xyl <- rgdal::project(uluru_bbox_trimesh$P[,1:2], projection(nt_raster), inv = TRUE)

## 2) image is merc, forwards to that
xym <- rgdal::project(xyl, projection(im))

## 3) the cell (in [0,1,0,1]) for our mesh coordinates
cell <- cellFromXY(im, xym)

## 4) use that cell to get native from a rescaled im
xyim <- xyFromCell(setExtent(im, extent(0, 1, 0, 1)), cell)

## 5) convert to RGB from palette (might be a raster fun for this...)
imrgb <- setValues(brick(im, im, im), t(col2rgb(im@legend@colortable[values(im) + 1])))

## 6) write to PNG (that's the only way we can texture map)
texfile <- sprintf("%s.png", tempfile())
rgdal::writeGDAL(as(imrgb, "SpatialGridDataFrame"), texfile, driver = "PNG")


## clear plot if need be
## rgl.clear()

## finally, plot the mesh as filled triangles and specify the texture mapping
## (it has to not be col = "black", which is the default )
shade3d(tri, texcoords = xyim[tri$it, ], texture = texfile, col = "white")
rglwidget()

## determine tile to request
## top left of our bounding box



## write to JSON
write_lines(mesh_json, "./data/uluru_mesh.json")
