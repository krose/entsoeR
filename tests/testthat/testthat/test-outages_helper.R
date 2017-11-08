
context("outages_helper")

test_that("outages_helper returns a tibble.",{
  
  # Prepare for tests by reading the file.
  xml_file_path <- system.file("outages/001-001-PLANNED_UNAVAIL_OF_GENERATION_UNITS_201711080000-201801010000.xml", 
                               package = "entsoeR")
  
  xml_file <- xml2::read_html(xml_file_path, encoding = "UTF-8")

  # Test returned class
  expect_equal(object = class(entsoeR:::outages_helper(xml_file)), 
               expected = c("tbl_df", "tbl", "data.frame"), label = "XML parsing.")
  
  # test colnames are identical.
  expect_equal(colnames(entsoeR:::outages_helper(xml_file)),
               c("mrid", "revisionnumber", "type", "process.processtype", "createddatetime", 
                 "sender_marketparticipant.mrid", "sender_marketparticipant.marketrole.type", 
                 "receiver_marketparticipant.mrid", "receiver_marketparticipant.marketrole.type", 
                 "unavailability_time_period.timeinterval", "timeseries", "point_series", 
                 "point", "reason_code", "reason_text"))
  
})
