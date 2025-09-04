import { test, expect } from "@playwright/test";

test("vira main page", async ({ page }) => {
  // Navigate to Vira main page
  await page.goto("/");

  // Verify the page loads successfully
  await expect(page).toHaveURL("/");

  // Check for page title
  await expect(page).toHaveTitle(/Vira/);

  // Verify navigation links are present
  await expect(page.getByRole("link", { name: /repositories/i })).toBeVisible();
  await expect(page.getByRole("link", { name: /settings/i })).toBeVisible();
});
