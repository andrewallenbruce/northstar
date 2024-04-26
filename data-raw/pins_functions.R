pin_update <- function(x, name, title, description) {

  board <- pins::board_folder(
    here::here("inst/extdata/pins")
    )

  board |>
    pins::pin_write(
      x,
      name        = name,
      title       = title,
      description = description,
      type        = "qs"
    )

  board |> pins::write_board_manifest()

}

list_pins <- function() {

  board <- pins::board_folder(
    here::here("inst/extdata/pins")
  )

  pins::pin_list(board)

}

delete_pins <- function(pin_names) {

  board <- pins::board_folder(
    here::here("inst/extdata/pins")
  )

  pins::pin_delete(board, names = pin_names)

}
