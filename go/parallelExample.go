package main

import (
  "log"
  "os"
  "sync"
  "fmt"
  "time"
  "strings"
  "io/ioutil"
  "github.com/PuerkitoBio/goquery"
)

func check(err error) {
  if err != nil {
    log.Println(err)
    os.Exit(1)
  }
}

func scrape(url string, titles chan string, wg *sync.WaitGroup) {
  doc, err := goquery.NewDocument(url)
  check(err)
  titles <- doc.Find("#section_0").Text()
  wg.Done()
}

func downloadUrlsSync(urls []string) {
  for _, url := range urls {
    doc, err := goquery.NewDocument(url)
    check(err)
    fmt.Println(doc.Find("#section_0").Text())
  }
}

func downloadUrlsAsync(urls []string) {
  var wg sync.WaitGroup
  titles := make(chan string, len(urls))
  for _, url := range urls {
    wg.Add(1)
    go scrape(url, titles, &wg)
  }
  fmt.Println("Waiting...")
  wg.Wait()
  fmt.Println("Done...")

  close(titles)
  for title := range titles {
    fmt.Println(title)
  }
}

func readFile(fileName string) []string {
  data, err := ioutil.ReadFile(fileName)
  check(err)
  lines := strings.Split(string(data), "\n")
  return lines[0 : len(lines)-1]
}

func main() {
  urls := readFile("urls.txt")
  fmt.Println("Async...")
  start := time.Now()
  downloadUrlsAsync(urls)
  end := time.Now()
  fmt.Println(end.Sub(start))


  fmt.Println("Async...")
  start = time.Now()
  downloadUrlsSync(urls)
  end = time.Now()
  fmt.Println(end.Sub(start))
}

