################################################################################

## Prerequisite code

################################################################################
library(devtools)
library(sf)
library(raster)
library(tidyverse)
library(RTriangle)
library(r2vr.gis) ## devtools::install("milesmcbain/r2vr.gis")

### Make a tight bounding box.
### coords come from a google map: https://drive.google.com/open?id=1Ak26Hyx1R-f2QjPCTK0rQLye5xcHyE8n&usp=sharing

uluru_bbox <-
  st_bbox(c(xmin = 131.02084,
            xmax = 131.0535,
            ymin = -25.35461,
            ymax = -25.33568),
          crs = st_crs("+proj=longlat +ellps=WGS84"))

### Read in raster
nt_raster <- raster("./data/ELVIS_CLIP.tif")

### Homogenise bbox and raster CRS
uluru_bbox_nt_crs<-
  bbox_transform(uluru_bbox, crs = crs(nt_raster)@projargs)

### Crop raster and sanity check
nt_raster_cropped <-
  raster_crop_bbox(nt_raster, uluru_bbox_nt_crs)

plot(nt_raster_cropped) ## looks good!

### Triangulate bbox
uluru_bbox_trimesh <-
  bbox_to_trimesh(uluru_bbox_nt_crs, 12000) # a few more than last example for finer mesh.

### Add elevation to trimesh
ul_extent_elev <-
  raster::extract(nt_raster, uluru_bbox_trimesh$P[, 1:2])
uluru_bbox_trimesh$P <-
  cbind(uluru_bbox_trimesh$P, ul_extent_elev)

### Calculating height correction factor for VR, discussed in previous post.
## We need to correct the height based on ground height.
## In this case we'll find ground height from the  highest corner of the bounding box.

bbox_corners <-
  bbox_to_multipoly(uluru_bbox_nt_crs)[[1]][[1]][[1]]

ground_height <- max(raster::extract(nt_raster, bbox_corners))

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
  a_json_model(src = uluru_json,
               scale = scale_factor * c(1, 1, 1),
               position = c(0, 0 + height_correction * scale_factor, -15),
               rotation = c(-90, 180, 0)
               )

sky <- a_entity(.tag = "sky",
                color = "#000000")

controls <- a_pc_control_camera()

aframe_scene <-
  a_scene(.template = "empty",
          .title = "Uluru Mesh",
          .description = "An A-Frame scene of Uluru",
          .children = list(uluru, sky, controls))

aframe_scene$serve()
browseURL("http://127.0.0.1:8080")

## don't forget to:
aframe_scene$stop()

## Smooth using vertex normals
uluru_smooth <-
  a_json_model(src = uluru_json,
               mesh_smooth = TRUE,
               scale = scale_factor * c(1, 1, 1),
               position = c(0, 0 + height_correction * scale_factor, -15),
               rotation = c(-90, 180, 0))

aframe_scene2 <-
  a_scene(.template = "empty",
          .title = "Uluru Mesh",
          .description = "An A-Frame scene of Uluru",
          .children = list(uluru_smooth, sky, controls))

aframe_scene2$serve()

## don't forget to:
aframe_scene2$stop()


### Using a texture
library(fs)
library(slippymath)
library(curl)
library(glue)

## Fetch a tile from mapbox calculate the tiles with slippymath
uluru_tiles <- bb_to_tg(uluru_bbox, max_tiles = 40)

mapbox_query_string <-
  paste0("https://api.mapbox.com/v4/mapbox.satellite/{zoom}/{x}/{y}.jpg90",
         "?access_token=",
         Sys.getenv("MAPBOX_API_KEY"))

images <-
  pmap(uluru_tiles$tiles,
       function(x, y, zoom){
         outfile <- glue("{x}_{y}.jpg")
         curl_download(url = glue(mapbox_query_string),
                       destfile = outfile) 
         outfile 
       },
       zoom = uluru_tiles$zoom)

## composite images with slippymath
uluru_raster <- tg_composite(uluru_tiles, images)

## Crop the image
uluru_raster <- raster_crop_bbox(uluru_raster, uluru_bbox)

## write to PNG (that's the only way we can texture map)
texfile <- "./data/uluru_satellite.png"
raster_to_png(uluru_raster, texfile)

## calculate the trimesh x and y in 0 - 1 space for texture coordinates
## using r2vr.gis::range_scale
xym <- uluru_bbox_trimesh$P[,1:2]

xyim <- apply(xym, 2, range_scale)
## This works because the image and the mesh have the same area and the area is
## relatively small, so the won't be much opportunity for the texture to be
## distorted by difference in projection.


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
                     .parts = "./data/uluru_satellite.png")

uluru <-
  a_json_model(src = uluru_tex,
               mesh_smooth = TRUE,
               scale = scale_factor * c(1, 1, 1),
               position = c(0, 0 + height_correction * scale_factor, -15),
               rotation = c(-90, 180, 0))

aframe_scene3 <-
  a_scene(.template = "empty",
          .title = "Uluru Mesh",
          .description = "An A-Frame scene of Uluru",
          .children = list(uluru, sky, controls))

aframe_scene3$serve()

## don't forget to:
aframe_scene3$stop()

## Bonus: quadmesh
## devtools::install_github("mdsumner/quadmesh")
library(quadmesh)

qm <- quadmesh(nt_raster_cropped)

triangulate_quads <- function(quad_index) {
  matrix(rbind(quad_index[c(1L, 4L, 2L), ], quad_index[c(4L, 3L, 2L), ]), 3L)
}

trimesh <- triangulate_quads(qm$ib)
## reuse same vertices with new indicies - cut quads in half along diagonals


vertices_columns <- t(qm$vb[1:3, ])
faces_columns <- t(trimesh)
texture_coords <- apply(vertices_columns[ , 1:2], 2, range_scale)

mesh_json <- trimesh_to_threejson(vertices = vertices_columns,
                                  face_vertices = faces_columns,
                                  vertex_uvs = texture_coords,
                                  texture_file = fs::path_file(texfile))

readr::write_file(mesh_json, "./data/uluru_tex.json")

## VR test
## The JSON references the satellite png file so it needs to be included in 'parts'.
uluru_q_tex <- a_asset(src = "./data/uluru_tex.json",
                       id = "uluru_tex",
                       .parts = "./data/uluru_satellite.png" )
uluru_quads <-
  a_json_model(src = uluru_q_tex,
               mesh_smooth = TRUE,
               scale = scale_factor * c(1, 1, 1),
               position = c(0, 0 + height_correction * scale_factor, -15),
               rotation = c(-90, 180, 0))

aframe_scene4 <-
  a_scene(.template = "empty",
          .title = "Uluru Mesh",
          .description = "An A-Frame scene of Uluru",
          .children = list(uluru_quads, sky, controls))

aframe_scene4$serve()


aframe_scene4$stop()


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

aframe_scene5 <-
  a_scene(template = "empty",
          title = "Uluru Mesh",
          description = "An A-Frame scene of Uluru",
          children = list(uluru, sky, a_pc_control_camera()))

aframe_scene5$serve()

## don't forget to:
aframe_scene5$stop()
