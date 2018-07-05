################################################################################

## Prerequisite code

################################################################################
library(devtools)
library(sf)
library(raster)
library(tidyverse)
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
  crop(st_bbox(uluru_bbox_mpoly)[c("xmin", "xmax",
                                   "ymin", "ymax")])

plot(nt_raster_cropped) ## looks good!

### Triangulate bbox
uluru_bbox_trimesh <-
  sf_to_trimesh(uluru_bbox_mpoly, 12000) # a few more than last example for finer mesh.

### Add elevation to trimesh
ul_extent_elev <-
  raster::extract(nt_raster, uluru_bbox_trimesh$P[, 1:2])
uluru_bbox_trimesh$P <-
  cbind(uluru_bbox_trimesh$P, ul_extent_elev)

### Calculating height correction factor for VR, discussed in previous post.
## We need to correct the height based on ground height.
## In this case we'll find ground height from the  highest corner of the bounding box.
ground_height <-
  max(raster::extract(nt_raster, uluru_bbox_mpoly[[1]][[1]][[1]], nrow = 1))

height_correction <- -1 * (ground_height - mean(uluru_bbox_trimesh$P[, 3]))
## We're reversing the correction that would have been applied to the
## ground height by centering.

################################################################################

## Shading a mesh in VR

################################################################################

## install latest r2vr using devtools
## install_github('milesmcbain/r2vr')

library(raster)
library(scico)
library(r2vr)
library(tidyverse)

## load JSON conversion helper function
source("./helpers/trimesh_to_threejson.R")

## load colour palette index helper function.
source("./helpers/vec_pal_colours.R")

## load vertex to face colour conversion
source("./helpers/vertex_to_face_colours.R")

## Extract data from raster for each vertex
## In this case we already have this data in uluru_bbox_trimesh$P[,3], so this
## is just for demonstration: if you are using a raster that isn't height, that
## will need to be done at this point.
colouring_raster_data <-
  raster::extract(nt_raster, uluru_bbox_trimesh$P[, 1:2])

## Choose a palette function to transform to colours
n_colours <- 256
palette_function <-
  purrr::partial(scico, palette = "tokyo")

## Generate colours
vertex_colour_data <-
  vec_pal_colours(colouring_raster_data, palette_function,
                    n_colours, zero_index = TRUE)

face_colours <-
  vertex_to_face_colours(vertex_colour_data$indexes,
                         uluru_bbox_trimesh$T)

## Generate a shaded model JSON
mesh_json <-
  trimesh_to_threejson(vertices = uluru_bbox_trimesh$P,
                       face_vertices = uluru_bbox_trimesh$T,
                       colours = vertex_colour_data$colours,
                       face_vertex_colours = face_colours)

## write JSON
write_file(mesh_json, "./data/uluru_mesh.json")

## Render in VR
## Bigger than our previous 'puddle':
scale_factor <- 0.01

uluru_json <-
  a_asset(id = "uluru",
          src = "./data/uluru_mesh.json")

uluru <-
  a_json_model(src_asset = uluru_json,
               scale = scale_factor * c(1, 1, 1),
               position = c(0, 0 + height_correction * scale_factor, -15),
               rotation = c(-90, 180, 0)
               )

sky <- a_entity(tag = "sky",
                color = "#000000")

controls <- a_pc_control_camera()

aframe_scene <-
  a_scene(template = "empty",
          title = "Uluru Mesh",
          description = "An A-Frame scene of Uluru",
          children = list(uluru, sky, controls))

aframe_scene$serve()
browseURL("http://127.0.0.1:8080")

## don't forget to:
aframe_scene$stop()

## Smooth using vertex normals
uluru_smooth <-
  a_json_model(src_asset = uluru_json,
               mesh_smooth = TRUE,
               scale = scale_factor * c(1, 1, 1),
               position = c(0, 0 + height_correction * scale_factor, -15),
               rotation = c(-90, 180, 0))

aframe_scene2 <-
  a_scene(template = "empty",
          title = "Uluru Mesh",
          description = "An A-Frame scene of Uluru",
          children = list(uluru_smooth, sky, controls))

aframe_scene2$serve()

## don't forget to:
aframe_scene2$stop()


### Using a texture
library(dismo)
library(fs)

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
texfile <- "./data/uluru_satellite.png"
rgdal::writeGDAL(as(imrgb, "SpatialGridDataFrame"), texfile, driver = "PNG")

## generate JSON containing texture
## pass just the name of the texture file so rendering process will look in the
## same directory as JSON model
uluru_tex_json <-
  trimesh_to_threejson(vertices = uluru_bbox_trimesh$P,
                       face_vertices = uluru_bbox_trimesh$T,
                       vertex_uvs = xyim,
                       texture_file = fs::path_file(texfile)
                       )

## write JSON file
readr::write_file(uluru_tex_json, "./data/uluru_tex.json")

## VR test
## The JSON references the satellite png file so it needs to be included in 'parts'.
uluru_tex <- a_asset(src = "./data/uluru_tex.json",
                     id = "uluru_tex",
                     parts = "./data/uluru_satellite.png")

uluru <-
  a_json_model(src_asset = uluru_tex,
               mesh_smooth = TRUE,
               scale = scale_factor * c(1, 1, 1),
               position = c(0, 0 + height_correction * scale_factor, -15),
               rotation = c(-90, 180, 0))

aframe_scene3 <-
  a_scene(template = "empty",
          title = "Uluru Mesh",
          description = "An A-Frame scene of Uluru",
          children = list(uluru, sky, controls))

aframe_scene3$serve()

## don't forget to:
aframe_scene3$stop()

## Bonus: rayshader
## devtools::install_github("diligently/rayshader")
library(rayshader)

elmat <- matrix(nt_raster_cropped@data@values, nrow = nt_raster_cropped@ncols,
                ncol = nt_raster_cropped@nrows)

elmat %>%
  sphere_shade(sunangle = 45, texture = "imhof2") %>%
  add_shadow(ray_shade(elmat,sunangle = 45), max_darken = 0.4) %>%
  add_shadow(ambient_shade(elmat), max_darken = 0.4) %>%
  write_png(filename = "./data/uluru_shade.png")

range_scale <- function(a) (a - min(a, na.rm=TRUE)) / diff(range(a, na.rm=TRUE))

norm_vertex_coords <-
  cbind(range_scale(uluru_bbox_trimesh$P[,1]),
        range_scale(uluru_bbox_trimesh$P[,2]))

texfile = "./data/uluru_shade.png"

uluru_tex_json <-
  trimesh_to_threejson(vertices = uluru_bbox_trimesh$P,
                       face_vertices = uluru_bbox_trimesh$T,
                       vertex_uvs = norm_vertex_coords,
                       texture_file = fs::path_file(texfile))


uluru_tex <- a_in_mem_asset(data = list(uluru_tex_json, readr::read_file_raw(texfile)),
                            src = "./uluru_tex.json",
                            id = "uluru_tex",
                            parts = "./uluru_shade.png")

aframe_scene4 <-
  a_scene(template = "empty",
          title = "Uluru Mesh",
          description = "An A-Frame scene of Uluru",
          children = list(uluru, sky, a_pc_control_camera()))

aframe_scene4$serve()

## don't forget to:
aframe_scene4$stop()
