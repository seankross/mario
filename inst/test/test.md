    suppressMessages(library(dplyr))
    library(mario)

    pipeline_call <- mtcars %>% 
      slice(1:20) %>% 
      select(mpg) %>% 
      parse_pipeline()

    pipeline_call

    ## mtcars %>% slice(1:20) %>% select(mpg)

    parse_pipeline(
      mtcars %>% 
      slice(1:20) %>% 
      select(mpg)
    )

    ## mtcars %>% slice(1:20) %>% select(mpg)

    mtcars %>% 
      slice(1:20) %>% 
      select(mpg) %>% 
      parse_pipeline()

    ## mtcars %>% slice(1:20) %>% select(mpg)
