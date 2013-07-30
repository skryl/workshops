package main

import (
  "encoding/xml"
  "log"
  "io/ioutil"
  "os"
  "fmt"
  "net/http"
)

type Line struct {
  Name string   `xml:"name"`
  Status string `xml:"status"`
}

type Service struct {
  Timestamp string  `xml:"timestamp"`
  TrainLines []Line `xml:"subway>line"`
}

func check(err error) {
  if err != nil {
    log.Println(err)
    os.Exit(1)
  }
}


func downloadUrl(url string) []byte {
  resp, err := http.Get(url)
  check(err)
  defer resp.Body.Close()
  data, err := ioutil.ReadAll(resp.Body)
  return data
}

func main() {
  data := downloadUrl("http://mta.info/status/serviceStatus.txt")
  service := new(Service)
  err := xml.Unmarshal(data, service)
  check(err)

  fmt.Println(service.Timestamp)
  for i, trainLine := range service.TrainLines {
    fmt.Printf("%d. Trainline %s is %s\n", i, trainLine.Name, trainLine.Status)
  }
}
