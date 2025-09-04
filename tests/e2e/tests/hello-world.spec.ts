import { test, expect } from '@playwright/test';

test('hello world', async ({ page }) => {
  // Navigate to a basic webpage
  await page.goto('data:text/html,<html><body><h1>Hello World</h1></body></html>');
  
  // Verify the page title
  await expect(page).toHaveTitle('');
  
  // Verify the heading content
  await expect(page.locator('h1')).toHaveText('Hello World');
});

test('basic page interaction', async ({ page }) => {
  // Create a simple page with minimal JavaScript
  await page.goto('data:text/html,<html><body><p id="test">Original</p><script>document.getElementById("test").textContent = "Changed";</script></body></html>');
  
  // Verify JavaScript executed
  await expect(page.locator('#test')).toHaveText('Changed');
});