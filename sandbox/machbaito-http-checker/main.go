package main

import (
	"bufio"
	"compress/gzip"
	"encoding/xml"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"path"
	"strings"
	"time"
)

const productionURL = "https://j-sen.jp/"
const epicStatgingURL = "https://j-sen.jp.epic.stg.lvs.space/"
const stagingURL = "https://j-sen.jp.stg.lvs.space/"

type SitemapIndex struct {
	Sitemaps []Sitemap `xml:"sitemap"`
}

type Sitemap struct {
	Loc string `xml:"loc"`
}

type URLSet struct {
	URLs []URL `xml:"url"`
}

type URL struct {
	Loc string `xml:"loc"`
}

func main() {
	sitemapURL := epicStatgingURL + "/sitemap/sitemap.xml"
	sitemapIndexText := httpGetByteText(sitemapURL)

	sitemapIndex := SitemapIndex{}
	if err := xml.Unmarshal(sitemapIndexText, &sitemapIndex); err != nil {
		panic(err)
	}

	for _, sitemap := range sitemapIndex.Sitemaps {
		gzipPath := downloadGzipXML(sitemap.Loc)
		urlSetText := extractGzipXMLByteText(gzipPath)

		urlSet := URLSet{}
		if err := xml.Unmarshal(urlSetText, &urlSet); err != nil {
			panic(err)
		}

		for _, pageURL := range urlSet.URLs {
			targetURL := strings.Replace(pageURL.Loc, productionURL, epicStatgingURL, 1)
			resp, err := http.Get(targetURL)
			if err != nil {
				fmt.Println(targetURL, err)
				continue
			}
			fmt.Println(resp.StatusCode, targetURL)
			time.Sleep(3)
		}
	}
}

func httpGetByteText(xmlURL string) []byte {
	resp, err := http.Get(xmlURL)
	if err != nil {
		panic(err)
	}

	body, err := ioutil.ReadAll(resp.Body)
	defer resp.Body.Close()
	return body
}

func downloadGzipXML(gzipURL string) string {
	resp, err := http.Get(gzipURL)
	if err != nil {
		panic(err)
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}

	_, fileName := path.Split(gzipURL)

	filePath := "/tmp/" + fileName

	file, err := os.OpenFile(filePath, os.O_CREATE|os.O_WRONLY, 0666)
	if err != nil {
		panic(err)
	}

	defer func() {
		file.Close()
	}()

	file.Write(body)

	return filePath
}

func extractGzipXMLByteText(path string) []byte {
	file, err := os.Open(path)
	defer file.Close()
	if err != nil {
		panic(err)
	}

	gzipReader, _ := gzip.NewReader(file)
	defer gzipReader.Close()

	scanner := bufio.NewScanner(gzipReader)
	results := []byte{}
	for scanner.Scan() {
		results = append(results, scanner.Bytes()...)
	}
	return results
}
