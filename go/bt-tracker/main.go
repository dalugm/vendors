package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"time"
)

var (
	trackerSources = map[string]string{
		"NGOSANG_ALL":     "https://ngosang.github.io/trackerslist/trackers_all.txt",
		"NGOSANG_ALL_IP":  "https://ngosang.github.io/trackerslist/trackers_all_ip.txt",
		"NGOSANG_BEST":    "https://ngosang.github.io/trackerslist/trackers_best.txt",
		"NGOSANG_BEST_IP": "https://ngosang.github.io/trackerslist/trackers_best_ip.txt",
		"XIU2_BEST":       "https://trackerslist.com/best.txt",
		"XIU2_ALL":        "https://trackerslist.com/all.txt",
		"XIU2_HTTP":       "https://trackerslist.com/http.txt",
	}

	btTrackerRegex = regexp.MustCompile(`(?m)^bt-tracker=.*$`)

	httpClient = &http.Client{
		Timeout: 30 * time.Second,
	}
)

func getTrackersFromURL(ctx context.Context, url string) ([]string, error) {
	req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
	if err != nil {
		return nil, err
	}

	resp, err := httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("bad status: %s", resp.Status)
	}

	var trackers []string
	scanner := bufio.NewScanner(resp.Body)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line != "" {
			trackers = append(trackers, line)
		}
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return trackers, nil
}

func updateAria2Trackers(trackers []string, path string) error {
	var configPath string
	if path == "" {
		homeDir, err := os.UserHomeDir()
		if err != nil {
			return fmt.Errorf("get home dir: %w", err)
		}
		configPath = filepath.Join(homeDir, ".aria2", "aria2.conf")
	} else {
		configPath = path
	}

	content, err := os.ReadFile(configPath)
	if err != nil {
		return fmt.Errorf("read config: %w", err)
	}

	contentStr := string(content)
	if !btTrackerRegex.MatchString(contentStr) {
		return fmt.Errorf("no bt-tracker configuration found")
	}

	newContent := btTrackerRegex.ReplaceAllString(
		contentStr,
		"bt-tracker="+strings.Join(trackers, ","),
	)

	tmpFile := configPath + ".tmp"
	if err := os.WriteFile(tmpFile, []byte(newContent), 0o644); err != nil {
		return fmt.Errorf("write temp file: %w", err)
	}

	if err := os.Rename(tmpFile, configPath); err != nil {
		os.Remove(tmpFile)
		return fmt.Errorf("rename file: %w", err)
	}

	return nil
}

func main() {
	printTrackers := flag.Bool("print", false, "print trackers to console")
	updateAria2Path := flag.String("update-aria2-path", "", "path to aria2 configuration file (default \"~/.aria2/aria2.conf\")")
	source := flag.String("source", "XIU2_BEST", "comma-separated sources: NGOSANG_{BEST,ALL}[_IP], XIU2_{BEST,ALL,HTTP}")
	flag.Parse()

	log.Println("Getting trackers ...")

	sources := strings.Split(*source, ",")

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	var wg sync.WaitGroup
	trackerChan := make(chan []string, len(sources))
	errChan := make(chan error, len(sources))

	for _, s := range sources {
		s := strings.TrimSpace(s) // Handle spaces like "a, b"
		url, exists := trackerSources[s]
		if !exists {
			log.Printf("Unknown source: %s", s)
			continue
		}

		wg.Add(1)
		go func(name, url string) {
			defer wg.Done()

			list, err := getTrackersFromURL(ctx, url)
			if err != nil {
				errChan <- fmt.Errorf("%s: %w", name, err)
				return
			}
			if len(list) > 0 {
				trackerChan <- list
			}
		}(s, url)
	}

	go func() {
		wg.Wait()
		close(trackerChan)
		close(errChan)
	}()

	var allTrackers []string
	for list := range trackerChan {
		allTrackers = append(allTrackers, list...)
	}

	for err := range errChan {
		log.Printf("Error: %v", err)
	}

	if *printTrackers {
		for _, t := range allTrackers {
			fmt.Println(t)
		}
		fmt.Printf("%d tracker(s) total\n", len(allTrackers))
		return
	}

	if len(allTrackers) == 0 {
		log.Fatal("No trackers fetched")
	}

	if err := updateAria2Trackers(allTrackers, *updateAria2Path); err != nil {
		log.Fatalf("Failed to update aria2: %v", err)
	}

	log.Println("Trackers Updated.")
}
