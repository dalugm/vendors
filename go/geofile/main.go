package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

var (
	geoFileSources = map[string][]string{
		"github": {
			"https://github.com/Loyalsoldier/v2ray-rules-dat/releases/latest/download/geoip.dat",
			"https://github.com/Loyalsoldier/v2ray-rules-dat/releases/latest/download/geosite.dat",
		},
		"cdn": {
			"https://cdn.jsdelivr.net/gh/Loyalsoldier/v2ray-rules-dat@release/geoip.dat",
			"https://cdn.jsdelivr.net/gh/Loyalsoldier/v2ray-rules-dat@release/geosite.dat",
		},
		"fastly": {
			"https://fastly.jsdelivr.net/gh/Loyalsoldier/v2ray-rules-dat@release/geoip.dat",
			"https://fastly.jsdelivr.net/gh/Loyalsoldier/v2ray-rules-dat@release/geosite.dat",
		},
	}

	httpClient = &http.Client{
		Timeout: 10 * time.Minute,
		Transport: &http.Transport{
			ResponseHeaderTimeout: 30 * time.Second,
		},
	}
)

func extractFilenameFromURL(rawURL string) (string, error) {
	u, err := url.Parse(rawURL)
	if err != nil {
		return "", err
	}

	filename := filepath.Base(u.Path)
	if filename == "." || filename == "/" {
		return "downloaded_file", nil
	}

	if idx := strings.Index(filename, "?"); idx != -1 {
		filename = filename[:idx]
	}

	return filename, nil
}

func downloadFile(ctx context.Context, urlStr, savePath string) error {
	filename, err := extractFilenameFromURL(urlStr)
	if err != nil {
		return fmt.Errorf("parse url: %w", err)
	}

	if savePath != "" {
		if err := os.MkdirAll(savePath, 0755); err != nil {
			return fmt.Errorf("create dir: %w", err)
		}
	}

	fullPath := filepath.Join(savePath, filename)
	tmpPath := fullPath + ".tmp"

	out, err := os.Create(tmpPath)
	if err != nil {
		return fmt.Errorf("create temp file: %w", err)
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, urlStr, nil)
	if err != nil {
		out.Close()
		os.Remove(tmpPath)
		return fmt.Errorf("create request: %w", err)
	}
	req.Header.Set("User-Agent", "go-geo-updater/1.0")

	resp, err := httpClient.Do(req)
	if err != nil {
		out.Close()
		os.Remove(tmpPath)
		return fmt.Errorf("http request: %w", err)
	}

	if resp.StatusCode != http.StatusOK {
		resp.Body.Close()
		out.Close()
		os.Remove(tmpPath)
		return fmt.Errorf("http status: %s", resp.Status)
	}

	written, err := io.Copy(out, resp.Body)
	resp.Body.Close()

	if err != nil {
		out.Close()
		os.Remove(tmpPath)
		return fmt.Errorf("save content: %w", err)
	}

	if err := out.Close(); err != nil {
		os.Remove(tmpPath)
		return fmt.Errorf("close file: %w", err)
	}

	if err := os.Rename(tmpPath, fullPath); err != nil {
		os.Remove(tmpPath)
		return fmt.Errorf("rename file: %w", err)
	}

	log.Printf("Downloaded %s (%d bytes)", filename, written)
	return nil
}

func downloadAll(ctx context.Context, urls []string, savePath string) []error {
	var wg sync.WaitGroup
	errChan := make(chan error, len(urls))

	for _, url := range urls {
		wg.Add(1)
		go func(u string) {
			defer wg.Done()
			if err := downloadFile(ctx, u, savePath); err != nil {
				errChan <- fmt.Errorf("%s: %w", u, err)
			}
		}(url)
	}

	go func() {
		wg.Wait()
		close(errChan)
	}()

	var errors []error
	for err := range errChan {
		errors = append(errors, err)
	}
	return errors
}

func main() {
	var (
		source = flag.String("source", "github", "Source: github, cdn, fastly")
		path   = flag.String("path", "", "Path to save downloaded files")
	)
	flag.Parse()

	urls, ok := geoFileSources[*source]
	if !ok {
		log.Fatalf("Unknown source: %s (available: github, cdn, fastly)", *source)
	}

	log.Println("Downloading geo files...")

	ctx := context.Background()
	errors := downloadAll(ctx, urls, *path)

	if len(errors) > 0 {
		log.Printf("Completed with %d errors:", len(errors))
		for _, err := range errors {
			log.Printf("  x %v", err)
		}
		os.Exit(1)
	}

	log.Println("All files downloaded successfully")
}
