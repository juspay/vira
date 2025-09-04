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
  // Create a simple interactive page
  await page.goto('data:text/html,<html><body><button id="btn">Click me</button><p id="result"></p><script>document.getElementById("btn").onclick = () => document.getElementById("result").textContent = "Button clicked!"</script></body></html>');
  
  // Click the button
  await page.click('#btn');
  
  // Verify the result
  await expect(page.locator('#result')).toHaveText('Button clicked!');
});