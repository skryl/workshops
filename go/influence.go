package main

import (
  "fmt"
  "log"
  "os"
  "github.com/PuerkitoBio/goquery"
)

func check(err error) {
  if err != nil {
    log.Println(err)
    os.Exit(1)
  }
}

func getInfluences(hostname, root string) {
  doc, err := goquery.NewDocument( hostname + root )
  check(err)
  doc.Find("tr").Each(func(i int, tr *goquery.Selection) {
    headerText := tr.Find("th").Text()
    if headerText != "Influenced" {
      return
    }
    tr.Find("a").Each(func(i int, a *goquery.Selection) {
      pageName, _ := a.Attr("href")
      fmt.Println(hostname + pageName)
    })
  })
}

func main() {
  hostname := "https://en.m.wikipedia.org/wiki/"
  getInfluences(hostname, "C_Programming_Language")
}
